-module(simplify_erlang).

-include("include/simplify_erlang.hrl").

-define(BASE_SANDBOX_URL, "https://sandbox.simplify.com/v1/api").
-define(BASE_LIVE_URL, "https://api.simplify.com/v1/api").

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
    ApiUrl = build_url(PublicKey),

    JwsHeader = #{
      <<"typ">> => <<"JWT">>,
      <<"alg">> => <<"HS256">>,
      <<"kid">> => list_to_binary(PublicKey),
      <<"api.simplifycommerce.com/uri">> => list_to_binary(ApiUrl),
      <<"api.simplifycommerce.com/timestamp">> => os:system_time(millisecond),
      <<"api.simplifycommerce.com/nonce">> => random:uniform(100000000)
     },

    {ok, { _ , _, Host, Port, _, _ }} = http_uri:parse(ApiUrl),
    JwsRequest = jws_signing:jws_encode(JwsHeader, Request, PrivateKey),
    {ok, ConnPid} = gun:open(Host, Port),
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

api_host("lvpb_" ++ _Rest) ->
    ?BASE_LIVE_URL;
api_host("sbpb_" ++ _Rest) ->
    ?BASE_SANDBOX_URL.


build_url(Key) ->
    api_host(Key) ++ "/payment".
