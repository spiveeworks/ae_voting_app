
-record(poll_vote,
        {id :: vanillae:account_id(),
         weight :: non_neg_integer()}).

-record(poll_option,
        {name :: string(),
         votes = [] :: [#poll_vote{}],
         vote_tally = 0 :: non_neg_integer()}).

% TODO: add fields for creator, curation status, etc.
-record(poll,
        {chain_id :: vanillae:contract_id(),
         creator_id :: vanillae:account_id(),
         category = 1 :: integer(),
         title :: string(),
         description :: string(),
         url = "" :: string(),
         % spec_ref = none :: none | vanillae:tx_hash(),
         close_height :: integer() | never_closes,
         closed :: boolean(),
         options :: #{integer() => #poll_option{}}}).

-record(registry,
        {version :: integer(),
         chain_id :: vanillae:contract_id()}).

