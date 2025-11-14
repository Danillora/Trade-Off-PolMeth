if(require(bibliometrix) == F) install.packages("bibliometrix"); require(bibliometrix)
if(require(here) == F) install.packages("here"); require(here)
if(require(openxlsx) == F) install.packages("openxlsx"); require(openxlsx)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(stringr) == F) install.packages("stringr"); require(stringr)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(scales) == F) install.packages("scales"); require(scales)
if(require(ggrepel) == F) install.packages("ggrepel"); require(ggrepel)

# setwd(here("data"))
# 
# M <- convert2df(list.files(), dbsource = "wos", format = "bibtex")
# M <- M %>% filter(PY != 2025) # 3864 registros eliminados

# setwd(here())

# saveRDS(M, "dataset psci.RDS")


M <- readRDS("dataset psci.RDS")

M <- M %>% mutate(TIAB = paste(TI, AB)) %>% 
  rownames_to_column(var = "id")

m_exp <- M %>% mutate(
  EXP = str_detect(TIAB, "FIELD EXPERIMENT|LABORATORY EXPERIMENT|LAB EXPERIMENT|VIGNETTE EXPERIMENT|CONJOINT EXPERIMENT|LIST EXPERIMENT|SURVEY EXPERIMENT|EXPERIMENTAL EVIDENCE|EXPERIMENTAL DESIGN|LAB IN THE FIELD EXPERIMENT|LAB-IN-THE-FIELD EXPERIMENT")
) %>% filter(EXP == 1)


m_obs <- M %>% filter(!id %in% m_exp$id) %>% mutate(
  OBS = str_detect(TIAB, "CROSS-SECTIONAL|PANEL DATA|TIME SERIES|TIME-SERIES-CROSS-SECTIONAL|TSCS|LONGITUDINAL DATA|CENSUS DATA|ELECTORAL DATA|REGISTER DATA|QUANTITATIVE ANALYS|STATISTICAL ANALYS|ECONOMETRIC ANALYS|COMPARATIVE STUDY|COMPARATIVE ANALYSIS|CASE STUDY|CASE STUDIES|PROCESS TRACING|QUALITATIVE COMPARATIVE ANALYSIS|QCA|REGRESSION DISCONTINUITY|RDD|DIFFERENCE-IN-DIFFERENCES|DIFFERENCES-IN-DIFFERENCES|INSTRUMENTAL VARIABLE|QUASI-EXPERIMENT|ETHNOGRAPHIC|HIERARCHICAL MODEL|MULTILEVEL ANALYSIS|MULTILEVEL MODEL|SPATIAL ANALYSIS|GEOGRAPHIC ANALYSIS|REGRESSION|LOGIT|BINOMIAL|PROBIT|COUNT DATA|MULTIVARIATE|SURVEY|SURVIVAL ANALYS|TEXT-AS-DATA|NETWORK ANALYS|BAYESIAN|MACHINE LEARNING|NATURAL LANGUAGE MODEL|SYNTHETIC CONTROL|PROPENSITY SCORE|INTERACTIVE MODEL|FUZZY SET|TRIPLE DIFFERENCES|INTERRUPTED TIME SERIES")
) %>% filter(OBS == 1)


M <- rbind(m_exp %>% mutate(EXP = NULL, TYPE = "EXP"),
           m_obs %>% mutate(OBS = NULL, TYPE = "OBS"))


a <- M %>% group_by(TYPE, PY) %>% summarise(n = n()) %>% 
  group_by(TYPE) %>% mutate(prop = n/sum(n))

theme_set(theme_bw())

ggplot(a, aes(x = PY, y = prop)) +
  geom_line(aes(color = TYPE)) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            size = 3, vjust = -.5, fontface = "bold",
            data = a %>% filter(PY == 2018 & TYPE == "OBS")) +
  geom_text_repel(aes(label = scales:: percent(prop, accuracy = 0.1)),
            size = 3,
            data = a %>% filter(PY %in% c(2010, 2015, 2024))) +
  scale_x_continuous(breaks = c(2010:2024)) +
  scale_y_continuous(labels = percent) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "top") +
  labs(x = "Publication Year", y = "% Annual Publication", color = "") +
  geom_label(aes(label = "Growth Rate:\nExp = 9.56\nObs = 1.68", x = 2010, y = 0.15), 
             size = 3, hjust = 0)

#dir.create("graphs and tables")

setwd(here("graphs and tables"))

ggsave("graph 1.png")


# publication venues


a <- M %>% group_by(SO, TYPE) %>% summarise(n = n()) %>% 
  arrange(TYPE, -n) %>% mutate(check = 1) %>%  
  group_by(TYPE) %>% mutate(rank = cumsum(check)) %>% 
  filter(rank <= 10)

setwd(here())

b <- read_xlsx("jif.xlsx") %>% rename(ABB = 2)

a <- a %>% left_join(b %>% select(`Journal name`, `5 Year JIF`, ABB) %>% mutate(`Journal name` = toupper(`Journal name`)), 
                     by = c("SO" = "Journal name"))
a <- a %>% group_by(TYPE) %>% mutate(prop = n/sum(n))

library(tidytext)



dist_so <- a %>% distinct(ABB, .keep_all = T) %>% 
  mutate(`5 Year JIF` = as.numeric(`5 Year JIF`))

round(mean(dist_so$`5 Year JIF`), 2)

a <- a %>% mutate(FI_CAT = ifelse(`5 Year JIF` >= 4.26, "Higher", "Lower"),
                  prop = percent(prop, accuracy = 0.1))

a <- a %>% mutate(ABB = reorder_within(ABB, n, TYPE))

ggplot(a, aes(x = ABB, y = n)) +
  geom_bar(stat = "identity", width = .6, aes(fill = FI_CAT)) +
  facet_wrap(~TYPE, scales = "free") +
  coord_flip() +
  geom_text(aes(label = paste0(n, " (", prop, ")")),
            size = 3, hjust = -.05) +
  scale_x_discrete(label = function(x) gsub("_.*", "", x)) +
  scale_y_continuous(expand = expansion(c(.1, .6))) +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 9)) +
  labs(y = "Published Papers", x = "Journal", fill = "Average IF")


a <- a %>% mutate(`5 Year JIF` = as.numeric(`5 Year JIF`))
ggplot(a, aes(x = n, y = `5 Year JIF`)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~TYPE, scales = "free")

a <- a %>% mutate(ABB = gsub("_.*", "", ABB))

ggplot(a, aes(x = n, y = `5 Year JIF`)) +
  geom_hline(yintercept = 4.26, color = "grey") +
  geom_smooth(method = "lm", 
              se = F, 
              color = "black", 
              lty = "dashed",
              size = .5) +
  geom_point() +
  geom_text_repel(aes(label = ABB), size = 2) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~TYPE, scales = "free_x") +
  labs(x = "Published Papers")

setwd(here("graphs and tables"))
ggsave("graph 2.png")

# rodagem padrao bibliometrix para exp---------

results_exp <- biblioAnalysis(m_exp, sep = ";")
results_obs <- biblioAnalysis(m_obs, sep = ";")

df <- rbind(
results_exp$CountryCollaboration %>% mutate(n = SCP + MCP, TYPE = "EXP") %>% head(20),
results_obs$CountryCollaboration %>% mutate(n = SCP + MCP, TYPE = "OBS") %>% head(20)
) %>% mutate(Country = str_to_title(Country)) %>% 
  mutate(Country = reorder_within(Country, n, TYPE)) %>% 
  group_by(TYPE) %>% mutate(prop = percent(n/sum(n), accuracy = 0.1))

ggplot(df, aes(x = Country, y = n)) +
  geom_bar(stat = "identity", width = .6, aes(fill = TYPE)) +
  geom_text(aes(label = paste0(n, " (", prop, ")")),
            size = 3, hjust = -.1) +
  facet_wrap(~TYPE, scales = "free") +
  coord_flip() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.position = "none") +
  scale_x_discrete(label = function(x) gsub("_.*", "", x)) +
  scale_y_continuous(expand = expansion(c(.1,.5))) +
  labs(y = "Published Papers", x = "Country")

ggsave("graph 3.png")


# NETWORK

NetMatrix <- biblioNetwork(m_exp, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network

png("graph 4_1 exp.png", width = 2400, height = 1800, res = 300)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
dev.off()

NetMatrix <- biblioNetwork(m_obs, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
png("graph 4_2 obs.png", width = 2400, height = 1800, res = 300)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
dev.off()


