{application, 'ae_voting_app', [
	{description, "Testing how to get cowboy set up in an Erlang project."},
	{vsn, "0.1.0"},
	{modules, ['counter','ae_voting_app','ae_voting_app_sup','hello_handler','restful']},
	{registered, [ae_voting_app_sup]},
	{applications, [kernel,stdlib,cowboy,vanillae]},
	{mod, {ae_voting_app, []}},
	{env, []}
]}.
