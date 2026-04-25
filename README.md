`spatialRegroup` is an R package designed to **evaluate and refine 
existing administrative partitions** (e.g. EPCI, districts, counties) 
by identifying spatial units that are statistically more similar to 
neighboring groups than to their own.

---

## Key Features

- **Border effect detection** : identifies municipalities whose 
  attributive profile is closer to a neighboring macro-unit than to 
  their own — revealing hidden cross-boundary similarities
  
- **Targeted reclassification** : only border units are reassigned, 
  preserving the integrity of well-classified interior units
  
- **Neighborhood spillover analysis** : quantifies attributive 
  affinity between adjacent macro-units, highlighting zones where 
  administrative boundaries do not reflect socio-economic realities
  
- **Isolate filtering** : automatically reintegrates statistically 
  isolated candidates back into their original group
  
- **Built-in model comparison** : evaluates whether the recomposed 
  partition better explains an outcome variable than the original 
  administrative partition, using multilevel models (AIC, ICC)
---
`spatialRegroup` tests whether these boundaries are **statistically 
coherent** by asking:

> *Are border municipalities more similar to their administrative 
> neighbors than to their own group ?*

This approach reveals **boundary effects** between macro-units : 
zones where two neighboring administrative entities share more 
socio-economic similarities than expected, suggesting that the 
administrative boundary may be cutting through a functionally 
homogeneous territory. 
---
