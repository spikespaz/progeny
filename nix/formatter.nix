{ writeShellApplication, fd, nixfmt, rustfmt, prettier }:
writeShellApplication {
  name = "treewide-formatter";
  runtimeInputs = [ fd nixfmt rustfmt prettier ];
  text = ''
    if [ "''${1:-}" = '--' ]; then
      exec nixfmt
    else
      fd "$@" -H -t file -e nix -X nixfmt '{}'
      fd "$@" -H -t file -e rs -X rustfmt '{}'
      fd "$@" -H -t file \
        -e yml -e yaml -e json \
        -X prettier --log-level error --write '{}'
    fi
  '';
}
