-record(credentials, { private_key, public_key }).

-record(card, { number, cardholder, expiry_month, expiry_year, cvc }).
-record(payment, { amount, currency, card }).
