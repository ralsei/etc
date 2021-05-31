let
  hyacinthKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIJ00fx+DUOMAJewv04dsAeHYbqxBNIBPpQjbVssGHZC";
in
{
  "password.age".publicKeys = [ hyacinthKey ];
}
