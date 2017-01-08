-module(jws_signing).

-export([jws_encode/3, jws_url_safe_decode_64/1,
         jws_url_safe_encode_64/1, hash_hmac/2, jws_sign/2]).


jws_encode(JwsHeader, Request, PrivateKey) ->
    Header = jws_url_safe_encode_64(binary_to_list(jsx:encode(JwsHeader))),
    Payload = jws_url_safe_encode_64(binary_to_list(Request)),
    Message = Header ++ "." ++ Payload,
    SignedMessage = jws_sign(PrivateKey, Message),
    Message ++ "." ++ SignedMessage.


jws_url_safe_decode_64(Bin) when is_binary(Bin) ->
    jws_url_safe_decode_64(binary_to_list(Bin));
jws_url_safe_decode_64(String) ->
    re:replace(re:replace(base64:decode(String), "-", "+"), "_", "/").


jws_url_safe_encode_64(Bin) when is_binary(Bin) ->
    jws_url_safe_encode_64(binary_to_list(Bin));
jws_url_safe_encode_64(String) ->
    re:replace(re:replace(re:replace(base64:encode(String), "\\+", "-", [global, {return, list}]),
                                        "/", "_", [global, {return, list}]),
                             "=", "", [global, {return, list}]).


hash_hmac(Message, Key) ->
    crypto:hmac(sha256, Key, Message).


jws_sign(PrivateKey, Message) ->
    DecodedKey = jws_url_safe_decode_64(PrivateKey),
    Signature = hash_hmac(Message, DecodedKey),
    jws_url_safe_encode_64(Signature).
