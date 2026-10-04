# v0.25 extension provenance

Repository-native code in this directory was assembled during the Food 5 GitHub refresh from accepted Food 4 packages and handbacks.

## Accepted sources

- `FoodDeserts_HFA_FigureKNN_Refresh_v1_0_1.zip`
  - SHA-256: `40d23ad30ce9d6c234a32cdf3794342eecf1700a3fd1eadd9cb37de90cecdf79`
- `FoodDeserts_HFA_FigureKNN_Refresh_Handback_20261003_153953.zip`
  - SHA-256: `5c1e3c9882fd54e0b34840f9346abc019ea305befc7609688065d4282d88093b`
- `FoodDeserts_HFA_CoreContrast_Robustness_v1_0.zip`
  - SHA-256: `4a0542b132a938e955d5f31bc692f5b07e00ff849c01d10117933c1da2d5014c`
- `FoodDeserts_HFA_CoreContrast_Robustness_Handback_20261003_180747.zip`
  - SHA-256: `81412128108f353c3a9d501f2287363cb6f2799b11ed27fdde45d75d92bc97b9`
- manuscript v0.25 package `FoodPolicy_Manuscript_v0_25_20261003.zip`
  - SHA-256: `6940b987458de68c85fee314a795f97b858f60ca05aa1f884452968b015c9a49`

## Repository-native adaptations

Package-specific preflight/handback wrappers containing local project paths are not copied into the public computational tree. Their scientific calculations, frozen benchmark files and fail-closed validation gates are retained in portable repository form.

The accepted Figure/KNN handback predates the manuscript's final independent-set Figure 4 presentation. The v0.25 bundle contains the exact final Figure 4 files and the exact benchmark data, but no standalone plotting source for that final presentation. `scripts/14_make_manuscript_outputs.R` therefore implements the final Figure 4 directly from those accepted numerical data. This is a provenance-preserving reimplementation of presentation code, not a new scientific analysis. Numeric gates require the accepted KNN series and exact independent-set benchmark before plotting.

Historical Figure 4b generation remains in Stage 14 for reproducibility, but v0.25 does not use Figure 4b.
