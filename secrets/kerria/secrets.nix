let
  kerriaKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPUY8iaRrDK7ramnZ/2SMOpU4kl+nUztxgxsnupS3Blo";
in
{
  "snmHashedPassword.age".publicKeys = [ kerriaKey ];
  "wgPrivkey.age".publicKeys = [ kerriaKey ];
  "nextcloudDbPass.age".publicKeys = [ kerriaKey ];
  "nextcloudPass.age".publicKeys = [ kerriaKey ];
  "bitwardenEnv.age".publicKeys = [ kerriaKey ];
}
