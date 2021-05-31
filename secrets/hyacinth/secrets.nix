let
  hyacinthKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK9urprpZE0aYvvK/LigIvgehzCNctpDa2rlAkUi8j/e";
in
{
  "wgPrivkey.age".publicKeys = [ hyacinthKey ];
}
