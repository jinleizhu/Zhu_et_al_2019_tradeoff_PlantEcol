velocity.fac.log <- function(seed.ht, z0, d, ref.ht) {
  # function to calculate wind interception parameter p.
  #
  # Args:
  #   seed.ht: seed height (vertical seed projection).
  #   z0: Aerodynamic roughness length.
  #   d: zero-plane replacement distance.
  #   ref.ht: reference height.
  ifelse(seed.ht > z0 + d, ((seed.ht - d) * log((seed.ht - d) / z0) - seed.ht + d + z0) /
           (seed.ht * log((ref.ht - d) / z0)), 0)
}
# this functions is used in script 4_0