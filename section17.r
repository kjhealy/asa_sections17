library(tidyverse)
library(ggtern)
library(ggrepel)

my_colors <- function (palette = "cb") {
    cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    rcb.palette <- rev(cb.palette)
    bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9",
                     "#009E73", "#F0E442", "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette)
    else if (palette == "rcb")
        return(rcb.palette)
    else if (palette == "bly")
        return(bly.palette)
    else stop("Choose cb, rcb, or bly ony.")
}

theme_set(theme_minimal())

sections <- read_csv("data/asa_sections_2017.csv")

sections <- sections %>% mutate(pct_students = (student_members/total_members)*100,
                                pct_regular = (regular_members/total_members)*100,
                                pct_retired = (retired_members/total_members)*100)

mean_students <- mean(sections$pct_students)
mean_retired <- mean(sections$pct_retired)
mean_regular <- mean(sections$pct_regular)
mean_female <- mean(sections$pct_female)

sections <- sections %>% mutate(students_pctdiff = pct_students - mean_students,
                                stu_flag = students_pctdiff > 0,
                                retired_pctdiff = pct_retired - mean_retired,
                                ret_flag = retired_pctdiff > 0,
                                fem_pctdiff = pct_female - mean_female,
                                fem_flag = fem_pctdiff > 0)

p <- ggplot(sections, aes(y = students_pctdiff,
                          x = reorder(section, students_pctdiff),
                          fill = stu_flag))

p + geom_col() + scale_fill_manual(values = my_colors("bly")) +
    guides(fill = FALSE) +
    labs(x = "",
         y = "Percentage Points Above or Below Average",
         title = "Percent Student Members by ASA Section, 2017",
         caption = "Data: ASA") +
    coord_flip()


p <- ggplot(sections, aes(x = total_members, y = pct_retired))

p + geom_point() + geom_smooth() +
    labs(x = "Total Members", y = "Percent Students",
         title = "Percent Student Members vs Section Size",
         caption = "Data: ASA")


p <- ggplot(sections, aes(y = retired_pctdiff,
                          x = reorder(section, retired_pctdiff),
                          fill = ret_flag))

p + geom_col() + scale_fill_manual(values = my_colors("rcb")) +
    guides(fill = FALSE) +
    labs(x = "",
         y = "Percentage Points Above or Below Average",
         title = "Percent Retired Members by ASA Section, 2017",
         caption = "Data: ASA") +
    coord_flip()


p <- ggplot(sections, aes(y = fem_pctdiff,
                          x = reorder(section, fem_pctdiff),
                          fill = fem_flag))

p + geom_col() + scale_fill_manual(values = my_colors("rcb")[6:5]) +
    geom_hline(yintercept = 0) +
    guides(fill = FALSE) +
    scale_y_continuous(breaks = seq(-32, 28, 10),
                       labels = c("20", "30", "40", "50", "60", "70", "80")) +
    labs(x = "",
         y = "Percent Female, Centered on Average of All Sections",
         title = "Percent Female by ASA Section, 2017",
         caption = "Data: ASA") +
    coord_flip()



p <- ggplot(sections, aes(x = pct_retired, y = pct_students))

p + geom_point() + geom_smooth() +
    labs(x = "Percent Retired", y = "Percent Students",
         title = "Percent Students vs Percent Retired",
         caption = "Data: ASA")


p <- ggplot(sections, aes(y = students_pctdiff,
                          x = reorder(section, total_members),
                          fill = stu_flag))

p + geom_col() + scale_fill_manual(values = my_colors("bly")) +
    guides(fill = FALSE) +
    labs(x = "",
         y = "Percentage Points Above or Below Average",
         title = "Percent Student Members by ASA Section, 2017",
         subtitle = "Sections are ordered from top to bottom by Total Membership",
         caption = "Data: ASA") +
    coord_flip()


p <- ggtern(data = sections,
            mapping = aes(y = pct_students, z = pct_female, x = pct_retired))

p0 <- p + geom_text(aes(label = sec), size = 2) +
    tern_limits(R = 0.9, T = 0.75, L = 0.55) +
    labs(y = "% Student", x = "% Retired", z = "% Female") +
    theme_bvbg() +
    theme_notitles() +
    theme(tern.panel.background = element_rect(fill = "white"),
          tern.panel.grid.major = element_line(linetype='solid', size = 0.1))

ggsave("figures/ternary.pdf", p0, scale = 1.15)
