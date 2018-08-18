# cloud-haskell

Build the project by running `stack build`.

To execute, run `stack exec -- cloud-haskell-exe` and provided the arguments required.

Add the additional nodes to `config.yaml`

`stack exec -- cloud-haskell-exe --help`:

```
An example of nodes sending messages to each other

Usage: cloud-haskell-exe --send-for INT --wait-for INT [--with-seed SEED]
  Run the cloud-haskell app

Available options:
  --send-for INT           How many seconds to send messages for
  --wait-for INT           Length of the grace period in seconds
  --with-seed SEED         Seed for RNGs
  -h,--help                Show this help text
```


