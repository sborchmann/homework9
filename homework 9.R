library(tidyverse)
library(babynames)

students <- read_csv("student_info.csv", show_col_types = FALSE) |>
  rename(
    name               = `Name`,
    year_of_study      = `year of study`,
    major              = `major`,
    experience_level   = `level of programming experience (e.g. beginner, intermediate, advanced)`,
    prog_languages     = `programming languages (R, Python, Julia, etc.)`,
    research_interests = `research / academic interests`,
    group              = `Group number / name`,
    attended_jan30     = `attendance jan 30th`
  )


# what is the distribution of research interests in the whole class?

students_interests <- students |>
  select(name, research_interests) |>
  separate_longer_delim(research_interests, regex("[;/]"))

students_interests <- students_interests |>
  mutate(
    research_interests=str_to_lower(str_trim(research_interests))
  )

ggplot(
  data = students_interests,
  mapping = aes(x = research_interests)
) + 
  geom_bar(na.rm=TRUE) +
  coord_flip()

# what is the average level of programming experience in each group?

valid_levels <- c("beginner", "intermediate", "advanced")

students |>
  mutate(
    experience_level = str_to_lower(str_trim(experience_level)),
    experience_level = case_when(
      str_detect(experience_level, "^mid-beginner") ~ "beginner",
      .default = experience_level
    ),
  experience_level = factor(experience_level, levels = valid_levels, ordered=TRUE),
) |>
      group_by(group) |>
      ggplot(aes(x=group, y=as.numeric(experience_level))) +
      geom_boxplot()

# how many people in each group know R, python, etc.?

students_languages <- students |>
  select(name, group, prog_languages) |>
  separate_longer_delim(prog_languages, regex("[,]"))

students_languages <- students_languages |>
  mutate(
    prog_languages=str_to_lower(str_trim(prog_languages))
  ) |>
group_by(group)

ggplot(
  data = students_languages,
  aes(fill=prog_languages, x=group,)
) + 
  geom_bar()

# do undergraduates have more or less programming experience than graduate students?

student_level <- students |>
  select(name, year_of_study, experience_level) |>
  group_by(year_of_study) |>
  mutate(
    year_of_study = case_when(
      str_detect(year_of_study, "year") ~ "graduate",
      str_detect(year_of_study, "year", negate = TRUE) ~ "undergraduate",
    )
  ) |>
  mutate(
    experience_level = str_to_lower(str_trim(experience_level)),
    experience_level = case_when(
      str_detect(experience_level, "^mid-beginner") ~ "beginner",
      .default = experience_level
    ),
    experience_level = factor(experience_level, levels = valid_levels, ordered=TRUE),
  )

ggplot(data = student_level, aes(x=year_of_study, y=as.numeric(experience_level))) +
  geom_boxplot()

  