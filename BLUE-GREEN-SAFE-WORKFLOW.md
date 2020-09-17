# Blue-Green-Safe workflow

**Note:** The Blue-Green-Safe workflow is meant to add a higher level of assurance that you can deploy new versions of your applications in a way that both the Old and New instances of your App work without hiccups during the interval of time when requests are still being processed or dispatched to the Old instance. However, **it does not ensure with 100% certainty that your deployments will always be safe, as we'll see.**

