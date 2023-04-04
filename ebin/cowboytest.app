{application, 'cowboytest', [
	{description, "Testing how to get cowboy set up in an Erlang project."},
	{vsn, "0.1.0"},
	{modules, ['counter','cowboytest_app','cowboytest_sup','hello_handler','restful']},
	{registered, [cowboytest_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {cowboytest_app, []}},
	{env, []}
]}.
