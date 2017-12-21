# cowboy_api_demo
Demo REST API using Erlang Cowboy
## Getting Started
### Prerequisites
#### rebar3
- OPTION 1 - binary
    + Download: https://s3.amazonaws.com/rebar3/rebar3
    + Make it executable:
      ```
      $ chmod +x rebar3
      ```
    + Put path to rebar3 to $PATH.
- OPTION 2 - building binary from source
    + Clone source and build:
      ```
      $ git clone https://github.com/erlang/rebar3.git
      $ cd rebar3
      $ ./bootstrap
      ```
    + Put path to PATH same as in Option 1
- OPTION 3 - following one of above options, then
    + Extract the contents of the escript to ~/.cache/rebar3/lib and create a shell scrip ~/.cache/rebar3/bin/rebar3:
      ```
      $ rebar3 local install
      ```
    + To upgrade to the latest script from s3 and install the same way:
      ```
      $ rebar3 local upgrade
      ```
### Quick Start
- Clone the repository
- Compile code and export script
  ```
  $ cd wapi/
  $ rebar3 release
  ```
- Start node:
  ```
  $ _build/default/rel/wapi/bin/wapi start
  ```
  Use 'help' instead of 'console' for all options.
- Open Emacs, temporarily load api-client.el file by:
  ```
  M-x load-file
  <path_to_api-client.el>
  ```
- In *\*scratch\**, select region which is an Erlang command string and **C-c a p i**. Result will be showed in *\*api-client/buffer\**.
- Stop node:
  ```
  $ _build/default/rel/wapi/bin/wapi stop
  ```

## Implementation Guides
- Create new application named 'wapi' ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/f2a99f470110ff6f8b1f46d8662f8e6af009839c))
  ```
  $ rebar3 new app wapi
  ```
- Add .edts ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/9c0bef5e3ea784649d931c474dc764c6ca07ff38))
- Add config for dependencies and others ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/e4be474103699dd43bbc2f2cf01b73e2318b6d4a))
  + [cowboy](https://hex.pm/packages/cowboy): a small, fast, modular HTTP server
  + [jsx](https://hex.pm/packages/jsx): for JSON encoding/decoding (not really necessary for this demo, but easier to use it for JSON manipulation)
  + Config for loading 'wapi' application when testing with 'rebar3 shell'
  + Config for 'rebar3 release' later
  + Fetch dependencies by
    ```
    rebar3 get-deps
    ```
  + Compiling code also includes fetching dependencies
    ```
    rebar3 compile
    ```
  + Running rebar3 shell also includes fetching dependencies and compiling code
    ```
    rebar3 shell
    ```
- Specify applications which should start before 'wapi' ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/5017416513471f833c611c6f9dc20a5e4ab1b4f1))
- Implement REST API Server ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/539f9ff6b62b5e08a8cb9c05a5292ca777ce73fa))
  + Set up listening for connections in [wapi/src/wapi_app.erl](https://github.com/mptnt1988/cowboy_api_demo/commit/539f9ff6b62b5e08a8cb9c05a5292ca777ce73fa#diff-d8cef3660559c870b805c5ec1b3d52fe).
    The handler is wapi_handler.erl.
    Find more details [here](https://ninenines.eu/docs/en/cowboy/2.2/guide/getting_started/).
  + Write some support functions in [wapi/src/wapi_lib.erl](https://github.com/mptnt1988/cowboy_api_demo/commit/539f9ff6b62b5e08a8cb9c05a5292ca777ce73fa#diff-1555a565a689fb607637a4acf50d3859) for evaluating Erlang command string, reading HTTP body, and formatting Erlang term to binary.
  + Implement [wapi/src/wapi_handler.erl](https://github.com/mptnt1988/cowboy_api_demo/commit/539f9ff6b62b5e08a8cb9c05a5292ca777ce73fa#diff-75189c35c68cf1e33d7c75de38bef2dd) which handles HTTP POST, parses the JSON body in form of
    ```
    {"cmd" : "<Erlang_cmd_string>"}
    ```
    evaluates the Erlang command string and returns result in JSON form
    ```
    {"ans" : "<result>"}
    ```
  + Log can be seen in Erlang shell.
- Implement Emacs REST API Client ([commit](https://github.com/mptnt1988/cowboy_api_demo/commit/d56e38c7d393c88c122a9ffe9d4a3de8a4fce209))
  + Emacs 'request' package required. It will be automatically installed by loading [api-client.el](https://github.com/mptnt1988/cowboy_api_demo/commit/d56e38c7d393c88c122a9ffe9d4a3de8a4fce209#diff-a2c7c3307cd19f5c58c51dd366f77086).
  + api-client-call function is bound to **C-c a p i**.

## References
[rebar3](https://www.rebar3.org/docs/getting-started)

[hex](https://hex.pm)

[Cowboy User Guide](https://ninenines.eu/docs/en/cowboy/2.2/guide/getting_started/)

[Cowboy Function Reference](https://ninenines.eu/docs/en/cowboy/2.2/manual)

  
