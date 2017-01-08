-module(simplify_erlang).

-include("include/simplify_erlang.hrl").

-define(BASE_URL, "sandbox.simplify.com").
-define(BASE_PORT, 443).

-define(USER_AGENT, "Erlang SDK 0.0.0").

%% API exports
-export([create_payment/2]).


%%====================================================================
%% API functions
%%====================================================================


%%
%% Creates a payment.
create_payment(#payment{amount=Amount,
                        currency=Currency,
                        card=#card{number=Number,
                                   cardholder=Name,
                                   expiry_month=ExpiryMonth,
                                   expiry_year=ExpiryYear,
                                   cvc=Cvc}},
               #credentials{private_key=PrivateKey,
                            public_key=PublicKey}) ->

    Request = jsx:encode(#{<<"amount">> => Amount,
                           <<"currency">> => Currency,
                           <<"card">> => #{
                               <<"number">> => Number,
                               <<"name">> => Name,
                               <<"expMonth">> => ExpiryMonth,
                               <<"expYear">> => ExpiryYear,
                               <<"cvc">> => Cvc}}),

    JwsHeader = #{
      <<"typ">> => <<"JWT">>,
      <<"alg">> => <<"HS256">>,
      <<"kid">> => list_to_binary(PublicKey),
      <<"api.simplifycommerce.com/uri">> => build_url(),
      <<"api.simplifycommerce.com/timestamp">> => os:system_time(millisecond),
      <<"api.simplifycommerce.com/nonce">> => random:uniform(100000000)
     },

    JwsRequest = jws_signing:jws_encode(JwsHeader, Request, PrivateKey),
    {ok, ConnPid} = gun:open(?BASE_URL, ?BASE_PORT),
    StreamRef = gun:post(ConnPid, "/v1/api/payment",
                         [ {<<"content-type">>, "application/json"},
                           {<<"Accept">>, "application/json"},
                           {<<"Authorization">>, "JWS " ++ JwsRequest },
                           {<<"User-Agent">>, ?USER_AGENT} ], JwsRequest),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _, _} ->
            no_data;
        {response, nofin, _, _} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            io:format("~s~n", [Body])
    end.



%%====================================================================
%% Internal functions
%%====================================================================

build_url() ->
    list_to_binary("https://" ++ ?BASE_URL ++ "/v1/api/payment").
