simplify-erlang
=====

An OTP library

Build
-----

    $ rebar3 compile




```
erl -env ERL_LIBS _build/default/lib -eval 'application:ensure_all_started(simplify_erlang).'
```


# Create a Payment

Only sandbox supported.

```
simplify_erlang:create_payment(#payment{amount=1000, currency = <<"USD">>,
                                            card = #card{
                                                number = <<"5555555555554444">>,
                                                expiry_month=12,
                                                expiry_year=20,
                                                cvc = <<"123">>,
                                                cardholder = <<"John Doe">> }},
                                   #credentials{private_key="[PRIVATE KEY]",
                                                public_key="[PUBLIC_KEY]"}).
```


# License

ASLv2. Copyright (c) 2017 Sébastien Le Callonnec
