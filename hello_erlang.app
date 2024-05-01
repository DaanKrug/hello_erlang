{application, hello_erlang,
[
  {description, "Hello Erlang App with OTP"},
  {vsn, "1"},
  {mod,{hello_erlang_app, []}},
  %{mod, {hello_erlang_sup, []}},
  {modules,[hello_erlang]},
  {registered, []},
  {applications,
    [
    kernel,
    stdlib,
	  jiffy,
	  cowboy,
	  eunit
    ]
  },
  {env, [{admin_emails, "daniel-krug@hotmail.com"}]}
]}.