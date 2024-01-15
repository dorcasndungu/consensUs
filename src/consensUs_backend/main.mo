actor FundAllocationContract = {
  proposals : [Proposal];

  // Define the Proposal type representing a fund allocation proposal
type Proposal = {
  proposalId : Nat;
  proposer : Principal;
  title : Text;
  description : Text;
  upvotes : Nat;
  isApproved : Bool;
};
shared actor class DAO(init : Types.BasicDaoStableStorage) = Self {
    stable var accounts = Types.accounts_fromArray(init.accounts);
    stable var proposals = Types.proposals_fromArray(init.proposals);
    stable var next_proposal_id : Nat = 0;
    stable var system_params : Types.SystemParams = init.system_params;

    system func heartbeat() : async () { 
        await execute_accepted_proposals();
    };
    func proposal_get(id : Nat) : ?Types.Proposal = Trie.get(proposals, Types.proposal_key(id), Nat.equal);
    func proposal_put(id : Nat, proposal : Types.Proposal) {
        proposals := Trie.put(proposals, Types.proposal_key(id), Nat.equal, proposal).0;
    };

/// Lists all accounts
    public query func list_accounts() : async [Types.Account] {
        Iter.toArray(
          Iter.map(Trie.iter(accounts),
                   func ((owner : Principal, tokens : Types.Tokens)) : Types.Account = { owner; tokens }))
    };

     /// Submit a proposal
    ///
    /// A proposal contains a canister ID, method name and method args. If enough users
    /// vote "yes" on the proposal, the given method will be called with the given method
    /// args on the given canister.
    public shared({caller}) func submit_proposal(payload: Types.ProposalPayload) : async Types.Result<Nat, Text> {
        Result.chain(deduct_proposal_submission_deposit(caller), func (()) : Types.Result<Nat, Text> {
            let proposal_id = next_proposal_id;
            next_proposal_id += 1;

            let proposal : Types.Proposal = {
                id = proposal_id;
                timestamp = Time.now();
                proposer = caller;
                payload;
                state = #open;
                votes_yes = Types.zeroToken;
                votes_no = Types.zeroToken;
                voters = List.nil();
            };
            proposal_put(proposal_id, proposal);
            #ok(proposal_id)
        })
    };
     /// Return the proposal with the given ID, if one exists
    public query func get_proposal(proposal_id: Nat) : async ?Types.Proposal {
        proposal_get(proposal_id)
    };
    /// Return the list of all proposals
    public query func list_proposals() : async [Types.Proposal] {
        Iter.toArray(Iter.map(Trie.iter(proposals), func (kv : (Nat, Types.Proposal)) : Types.Proposal = kv.1))
    };
    // Vote on an open proposal
    public shared({caller}) func vote(args: Types.VoteArgs) : async Types.Result<Types.ProposalState, Text> {
        switch (proposal_get(args.proposal_id)) {
        case null { #err("No proposal with ID " # debug_show(args.proposal_id) # " exists") };
        case (?proposal) {
                 var state = proposal.state;
                 if (state != #open) {
                     return #err("Proposal " # debug_show(args.proposal_id) # " is not open for voting");
                 };
                 switch (account_get(caller)) {
                 case null { return #err("Caller does not have any tokens to vote with") };
                 case (?{ amount_e8s = voting_tokens }) {
                          if (List.some(proposal.voters, func (e : Principal) : Bool = e == caller)) {
                              return #err("Already voted");
                          };
                          
                          var votes_yes = proposal.votes_yes.amount_e8s;
                          var votes_no = proposal.votes_no.amount_e8s;
                          switch (args.vote) {
                          case (#yes) { votes_yes += voting_tokens };
                          case (#no) { votes_no += voting_tokens };
                          };
                          let voters = List.push(caller, proposal.voters);

                          if (votes_yes >= system_params.proposal_vote_threshold.amount_e8s) {
                              // Refund the proposal deposit when the proposal is accepted
                              ignore do ? {
                                  let account = account_get(proposal.proposer)!;
                                  let refunded = account.amount_e8s + system_params.proposal_submission_deposit.amount_e8s;
                                  account_put(proposal.proposer, { amount_e8s = refunded });
                              };
                              state := #accepted;
                          };
                          
                          if (votes_no >= system_params.proposal_vote_threshold.amount_e8s) {
                              state := #rejected;
                          };

                          let updated_proposal = {
                              id = proposal.id;
                              votes_yes = { amount_e8s = votes_yes };                              
                              votes_no = { amount_e8s = votes_no };
                              voters;
                              state;
                              timestamp = proposal.timestamp;
                              proposer = proposal.proposer;
                              payload = proposal.payload;
                          };
                          proposal_put(args.proposal_id, updated_proposal);
                      };
                 };
                 #ok(state)
             };
        };
    };
    /// Execute all accepted proposals
    func execute_accepted_proposals() : async () {
        let accepted_proposals = Trie.filter(proposals, func (_ : Nat, proposal : Types.Proposal) : Bool = proposal.state == #accepted);
        // Update proposal state, so that it won't be picked up by the next heartbeat
        for ((id, proposal) in Trie.iter(accepted_proposals)) {
            update_proposal_state(proposal, #executing);
        };

        for ((id, proposal) in Trie.iter(accepted_proposals)) {
            switch (await execute_proposal(proposal)) {
            case (#ok) { update_proposal_state(proposal, #succeeded); };
            case (#err(err)) { update_proposal_state(proposal, #failed(err)); };
            };
        };
    };
