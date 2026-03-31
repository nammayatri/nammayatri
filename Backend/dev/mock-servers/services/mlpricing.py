"""ML Pricing (congestion charge) mock."""


def handle(handler, path, body):
    # POST /mlpricing/internal/getCongestionCharge
    # Returns no congestion multiplier — keeps fare unchanged
    handler._json({"congestionChargeMultiplier": None})
