# Data sources and vintages

Raw/provider data and the assembled 33,755-row analytical input are **not
redistributed** in this repository. Reproduction starts from the documented
provider sources and reconstructs the analytical frame locally.

## Sources

| Source | Vintage | Public acquisition used by the code | Role |
|---|---|---|---|
| ONS LSOA boundaries | December 2021 BGC V5 | ONS ArcGIS FeatureServer `Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/FeatureServer/0` | Canonical LSOA21 geography and spatial closure |
| ONS LSOA population-weighted centroids | 2021 V4 | ONS ArcGIS FeatureServer `LSOA_PopCentroids_EW_2021_V4/FeatureServer/0` | Representative locations for retail-distance construction |
| Census TS045 | Census 2021 | `https://www.nomisweb.co.uk/output/census/2021/census2021-ts045.zip` | Household car availability / transport-constraint axis |
| Census TS011 | Census 2021 corrected table | `https://www.nomisweb.co.uk/output/census/2021/census2021-ts011.zip` | Source-reconstruction material capability baseline |
| DfT JTS0507 | 2019 | `https://assets.publishing.service.gov.uk/media/6182b95bd3bf7f56003e98e0/jts0507.ods` | Historical food-store journey-time comparator |
| DfT Transport Connectivity Metric | 2025 | `https://assets.publishing.service.gov.uk/media/68c966fc07d9e92bc5517b80/connectivity_metrics_2025.ods` | Shopping accessibility readouts |
| GEOLYTIX Retail Points reconstruction | Q1 2023 | ArcGIS FeatureServer `Geolytix_Retail_Points_Q1_2023/FeatureServer/7` | Retail supply and nearest qualifying large-store distance |
| English Indices of Deprivation | 2025 corrected File 7 | GOV.UK asset `File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv` | Income Deprivation Domain score/rate / material-constraint axis |
| ONS Rural Urban Classification | 2021 | ArcGIS item `9dbf7613cbb147b8bb8627ddb3568cff`, layer 0 | Nominal contextual readout |
| Internet User Classification | 2018 | Primary source: GeoDS IUC dataset; accepted run used the validated GLA/LOTI ArcGIS mirror when direct GeoDS retrieval was unavailable | Nominal contextual readout |
| ONS LSOA11–LSOA21 lookup | exact-fit lookup used by the accepted scripts | ONS Open Geography download API | Harmonisation of LSOA11 sources |

## Accepted source identities

The successful clean-room run recorded byte identities where stable downloadable
files were used. Important examples include:

- Census TS045 ZIP: SHA-256 `95593dcd53247d4458e97edd7454577a63e47fd43be957f0a2460e872efdc070`
- Census TS011 ZIP: SHA-256 `e7133720f12ed092580e2b815ca6a522f51adfe9e7b2fe272c4a7eefd26389af`
- DfT JTS0507 ODS: 29,144,861 bytes; SHA-256 `b0dc4e2c3d58056e37ec071eca85a42767c7008fb818477fe3001ee53534a87d`
- DfT TCM 2025 ODS: 68,164,574 bytes; SHA-256 `4f6589aeb3b3f51a26dae6d9d27c45188c36512a3dc052d848b4dc665cb16476`

Service-backed resources are validated by schema, population and/or recorded
geometry/source contracts rather than by assuming that a mutable service export
will remain byte-identical forever.

`provenance/discovered_source_urls.txt` lists URLs detected from the accepted
source scripts. `provenance/source_records/` contains compact source registers
and manifests retained from the successful clean-room run.

## Internet User Classification

The primary conceptual source is the **GeoDS Internet User Classification
2018**. The accepted clean-room run obtained the categorical classification
through the public GLA/LOTI ArcGIS mirror after validating:

- 41,729 unique GB LSOA/Data Zone identifiers;
- 32,844 England LSOA11 identifiers;
- exactly 10 IUC groups;
- non-missing group labels.

The IUC is retained as a nominal classification and is never treated as an
ordinal 1–10 scale.

## Data-assembly provenance

Source assembly and preparation were originally developed on a MacBook. The
TDABM analysis, de-patched clean worktree and final source-to-paper clean-room
replication were executed on the Linux analysis workstation. Machine-specific
filesystem paths have been removed from the public release; this does not alter
source identities or analytical results.

## Running the acquisition/reconstruction pipeline

From the repository root:

```bash
bash run_all.sh
```

The source stages download public resources where supported and fail closed when
the expected source/version/schema is unavailable. If a provider requires manual
retrieval, the stage reports the expected location and filename.

Do not silently substitute a newer data release. A reproduction of the paper
should use the vintages and source identities above. A future update using newer
sources should be treated as a new analytical release.

## Redistribution

Provider terms can restrict redistribution even when acquisition is public.
The repository therefore distributes code, source specifications, hashes and
compact provenance records rather than provider files or the assembled analytical
dataset.
