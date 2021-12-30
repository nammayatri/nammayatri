let producerCfg =
  { brokers = ["localhost:29092"]
  }

in

{ producerCfg = producerCfg
, envCfgs = envCfgs
}