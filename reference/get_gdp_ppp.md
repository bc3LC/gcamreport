# get_gdp_ppp

Retrieves GDP (PPP) data, computes regional GDP and annual GDPpc growth
rate, and converts units to 10 USD.

## Usage

``` r
get_gdp_ppp(GCAM_version = "v8.2")
```

## Arguments

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

## Value

\`GDP_PPP_clean\`, \`GDP_PPP_pc_growth_clean\`, and
\`GDP_PPP_pc_oecd_share_clean\` global variables.
