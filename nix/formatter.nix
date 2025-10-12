{ writeShellApplication, fd, nixfmt, rustfmt }:
writeShellApplication {
  name = "treewide-formatter";
  runtimeInputs = [ fd nixfmt rustfmt ];
  text = ''
    if [ "''${1:-}" = '--' ]; then
      exec nixfmt
    else
      fd "$@" -H -t file -e nix -X nixfmt '{}'
      fd "$@" -H -t file -e rs -X rustfmt '{}'
    fi
  '';
}
