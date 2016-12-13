# Taser

A *really* simple synchronous wrapper around `gun`.

Features:

- No frills synchronous API
- Follow redirects automatically
- Various timeout settings
- Deflates gzipped data
- Support for basic auth

Examples:

    {ok, StatusCode, RespHeaders, Body} = taser:get(URL).
    {ok, StatusCode, RespHeaders, Body} = taser:post(URL, Data).

    {ok, StatusCode, RespHeaders, Body} = taser:request(post, URL, Headers, #{
        connect_timeout => 5000,
        response_timeout => 5000,
        follow_redirects => true,
        max_redirects => 5,
        data => <<"Hello world!">>
    }).

    {ok, StatusCode, RespHeaders, Body} =
        taser:get("http://user:passwd@httpbin.org/basic-auth/user/passwd").

## TODO:

- Check status on gzip bombs
- Implement all common verbs
- Automatically format payloads and inject proper content type (form encoded,
  json, files?)
- Max body size
- Send timeout?
- Tests and stuff
- Option to skip body?
