# Fiscal_capacity_convergence
# Set up =============
pacman::p_load(readxl, janitor, dplyr, tidyr, stringr, constructive)

# Load data ====
states_finances_raw <- read_excel(
  here(raw, "State Finances - Key Fiscal Indicators.xlsx")
) |>
  slice(-c(1:2))

# Identify column names
colnames_state_finances <- colnames(states_finances_raw)
# Extract year
year <- str_extract(colnames_state_finances, "^\\d{4}")

# Identify columns containing year information
is_year <- str_detect(colnames_state_finances, "^\\d{4}\\.\\.\\.\\d+$")

# Identify group/state columns
is_group <- !is_year & !colnames_state_finances %in% c("S.N", "Description")

# Extract group names
group <- if_else(is_group, colnames_state_finances, NA_character_)

# Carry group name forward
group <- tidyr::fill(
  tibble(group),
  group
)$group

states_finances <- states_finances_raw %>%
  select(
    `S.N`,
    Description,
    matches("^\\d{4}\\.\\.\\.\\d+$")
  ) %>%
  rename_with(
    ~ paste(
      group[is_year],
      str_extract(.x, "^\\d{4}"),
      sep = "_"
    ),
    matches("^\\d{4}\\.\\.\\.\\d+$")
  )


# write intermediate file
write.csv(states_finances, here(inter, "state_finances_inter.csv"))

