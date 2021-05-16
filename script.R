if(!require(pacman))install.packages("pacman")

pacman::p_load("udpipe", "dplyr", "ggplot2", "sysfonts", "extrafont", "showtext", "glue")

dir.create("custom_fonts")

download_google_font <- function(font_family_name) {
  base_url <- "https://fonts.google.com/download?family={font_family_name}"
  
  download.file(glue(base_url), destfile = glue("custom_fonts/{font_family_name}.zip"), mode = "wb")
  
  unzip(glue("custom_fonts/{font_family_name}.zip"),
        exdir = glue("custom_fonts/{font_family_name}"))
}

download_google_font("Quando")
download_google_font("Karla")

sysfonts::font_add(family = "karla", regular = "custom_fonts/Karla/static/Karla-Regular.ttf")
sysfonts::font_add(family = "quando", regular = "custom_fonts/Quando/Quando-Regular.ttf")

showtext::showtext_auto()

`%notin%` <- purrr::negate(`%in%`)

theme_set(theme_minimal() +
            theme(axis.line.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "#dddddd", size = 0.9, linetype = 2),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(face = "bold"),
                  text = element_text(family = "karla"),
                  plot.title = element_text(family = "quando")))

file_url <- "https://raw.githubusercontent.com/UniversalDependencies/UD_Portuguese-GSD/master/pt_gsd-ud-train.conllu"

pt_br_annotation <- udpipe_read_conllu(file_url)

removed_upos <- c("PUNCT", "X", "SYM")

term_frequency <- pt_br_annotation %>%
  filter(upos %notin% removed_upos, !is.na(upos), !is.na(lemma)) %>%
  group_by(lemma) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  mutate(prop = freq/sum(freq), rank = row_number())

term_frequency %>%
  filter(rank <= 50) %>%
  ggplot(aes(x = rank, y = freq)) +
  geom_line(size = 1.7, color = "#118ab2") +
  geom_col(alpha = 0.3, fill = "#118ab2") +
  geom_hline(yintercept = 0, size = 1.1) +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(y = "Frequência das palavras", x = "Rank",
       caption = "Fonte dos dados: Universal Dependencies Portuguese GSD Corpus",
       title = "A frequência das palavras da língua portuguesa\nsegue a lei de Zipf",
       subtitle = "Essa distribuição estatística mostra uma relação inversamente\nproporcional entre a frequência das palavras e seu rank")
