library(shiny)
library(shinydashboard)
library(tidyverse)
library(extraDistr)
require(rgdal)
devtools::load_all() # loading my functions

# Database------

## Loading database------

data("data_1")
## Pag1------
month_names <-
  factor(
    month.abb,
    levels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  )
database <- data_1 %>%
  filter(Station == "1") %>%
  mutate(
    month_names = rep(month_names, 21) %>% rep(14),
    data = paste0(Year, "/", month_names),
    t = seq(1, 252) %>% rep(14),
    Group = rep(paste0("Group ", c(
      1, 2, 3, 3, 2, 1, 1, 3, 2, 3, 3, 3, 1, 1
    )), each = 252),
    Latitude = scale(Latitude %>% as.numeric()),
    Longitude = scale(Longitude %>% as.numeric()),
    cost = cos(2 * pi * as.numeric(Month) / 12),
    sent = sin(2 * pi * as.numeric(Month) / 12)
  )

TT <- 120
database <- database %>%
  group_by(City) %>%
  slice(1:TT) %>%
  data.frame()
group <- database["Group"] %>%
  unlist() %>%
  unique()


#------ Pag2 -----
path_estimation <- "estimations_predicts/estimations.rds"
results <- readRDS(path_estimation)

p <- results$p
q <- results$q
database_2 <- results$data %>%
  arrange(Group) %>%
  group_by(City) %>%
  slice(-(1:(max(q, p))))
quantil <- results$quantil

cov_kl <- results$cov_kl
cov_delta <- results$cov_delta
ncx <- ncol(cov_kl)
ncv <- ncol(cov_delta)

alfa.est <- results$alpha[, 1]

est <- results$DADOS %>%
  data.frame() %>%
  mutate(
    bq_t = 1 / delta,
    phi_ts = log(kl_t) / log(1 - (1 - quantil) ^ (delta)),
    aq_t = 1 / phi_ts,
    t = database_2$t,
    UR = y,
    Mês = database_2$month_names,
    Ano = database_2$Year,
    LONGITUDE = database_2$Longitude,
    LATITUDE = database_2$Latitude
  ) %>%
  arrange(grupos)


estt <- NULL

n <- 1000
estt <- NULL
B <- 50
mtrim <- .22
yest.aux <- matrix(0, nrow = nrow(est), ncol = B)
for (B in 1:B) {
  estt <- NULL
  for (j in 1:length(alfa.est)) {
    # qest=NULL
    G <- paste("Group", j)
    alfa.aux <- alfa.est[j]
    # set.seed(10)
    e <- est %>%
      dplyr::filter(grupos == G) %>%
      mutate(
        zm = rstable_pos(n, alfa.aux) %>% mean(trim = mtrim),
        H = zm * weights,
        Espy_z = bq_t * H * beta(1 + 1 / aq_t, bq_t * H),
        Vary_z = bq_t * H * beta(1 + 2 / aq_t, bq_t * H) - (bq_t * H * beta(1 + 1 / aq_t, bq_t * H)) ^
          2,
        betaa = rbeta(1, H, 1),
        yest = (1 - betaa ^ (delta)) ^ (phi_ts),
        ycond = (1 - (1 - quantil) ^ (1 / (bq_t * H))) ^ (1 / aq_t),
        ymarg = (1 - exp(
          -(delta / weights) * (-log(1 - quantil)) ^ (1 / alfa.aux)
        )) ^ phi_ts,
        Fy_z = pkumar(.7, aq_t, bq_t * zm * weights, lower.tail = F),
        r_qt = qnorm(pkumar(y, aq_t, bq_t * zm * weights, lower.tail = F)),
        cofvar = sqrt(Vary_z) / Espy_z,
      )


    estt <- rbind(estt, e)
  }
  yest.aux[, B] <- estt$yest
}
# yest <- apply(yest.aux, 1,median)
estt <- estt %>%
  mutate(yest = apply(yest.aux, 1, median)) %>%
  arrange(grupos)
# est = estt %>% mutate(erro1=round(((UR-yest)/UR),2),erro2=scale(UR-yest))


# pag3 --------------------------------------------------------------------

path.map <- "map_AM/13mu2500gc.shp"
am <- readOGR(path.map,encoding ="UTF-8")
cidades <- am[][[2]]

ui <- dashboardPage(
  title = "Dashboard",
  dashboardHeader(title = ""),
  dashboardSidebar(sidebarMenu(
    menuItem(text = "Descriptive", tabName = "pag1"),
    menuItem(text = "Predicts/Probabilitys", tabName = "pag2"),
    menuItem(text = "Maps", tabName = "pag3")
  )),
  dashboardBody(
    h1(
      "Results of Article: A marginal model for spatial-temporal
prediction and exceedance of relative humidity (In progress)"
    ),
    h2("Nascimento, L. B. F.; Lima, M.S. and Duczmal, L. H."),
    tabItems(
      tabItem(
        tabName = "pag1",
        h1("Datas/Statistics"),
        fluidRow(box(
          title = "Groups",
          width = 12,
          selectInput(
            inputId = "group",
            label = "Choose Group",
            choices = group
          )
        )),
        fluidRow(column(width = 12,
                        plotOutput("figure_RH"))),
        fluidRow(column(
          width = 12,
          tableOutput("table_sum"),
          offset = 5
        )),
        fluidRow(column(width = 12,
                        plotOutput("figure_cor_RH"))),
        fluidRow(
          column(
            width = 12,
            DT::dataTableOutput("table_data")
          ))
      ),
      tabItem(
        tabName = "pag2",
        #h1("Predicts/Probabilitys"),
        fluidRow(
          box(
            title = "Groups",
            width = 12,
            selectInput(
              inputId = "group_est",
              label = "Choose Group",
              choices = group
            )
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              "RH: Relative humidity \n
              RH_est: Predict Relative humidity ",
              title = "Predicts values",
              plotOutput(
                "figure_predict_RH"
              )
            ),

            tabPanel(
              "Probability of Relative humidity greater than 0.70",
              title = "Probabilitys",
              plotOutput(
                "figure_prob_RH"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "pag3",
        h2("Maps (In progress)"),
        fluidRow(
          box(
            width = 12,
            selectInput(
              inputId = "city",
              label = "Choose a city",
              choices = sort(cidades)
            ),
            actionButton(
              inputId = "botao",
              label = "Send"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("grafico_maps")
          )
        )
      )
    )
  )
)

        server <- function(input, output, session) {
          output$figure_RH <- renderPlot({
            database %>%
              filter(stringr::str_detect(Group, input$group)) %>%
              ggplot(.) +
              geom_line(aes(t, RH)) +
              geom_point(aes(t, RH)) +
              # ylab("Relative humidity")+ scale_y_continuous(labels=scales::percent)+
              # scale_x_discrete(labels = c(month.abb))+
              facet_grid(~ City) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 90, size = 8))
          })


          output$table_sum <- renderTable({
            aux.data <- database %>%
              filter(stringr::str_detect(Group, input$group)) %>%
              group_by(City) %>%
              summarise(Mean = mean(RH),
                        Median = median(RH),
                        Sd = sd(RH))
            aux.data
          })

          output$figure_cor_RH <- renderPlot({
            aux.data <-
              database %>% filter(stringr::str_detect(Group, input$group))
            Lm <- length(aux.data$City %>% unique())
            matrix.data <- matrix(aux.data$RH, nrow = TT, ncol = Lm)
            colnames(matrix.data) <- c(unique(aux.data$City))
            corrplot::corrplot(cor(matrix.data), method = "number")
          })

          output$table_data <- DT::renderDataTable({
            aux.data <-
              database %>% filter(stringr::str_detect(Group, input$group))
            aux.data
          })

          output$figure_predict_RH <- renderPlot({
            estt %>%
              filter(stringr::str_detect(grupos, input$group_est)) %>%
              rename(RH = UR, RH_est = yest) %>%
              select(t, RH, RH_est, grupos, idx) %>%
              pivot_longer(cols = c(RH_est, RH)) %>%
              data.frame() %>%
              ggplot(., aes(colour = name)) +
              geom_line(aes(t, value)) +
              # geom_point(aes(t,value))+
              ylab("Relative humidity") + # scale_y_continuous(labels=scales::percent)+
              # scale_x_discrete(labels = c(month.abb))+
              facet_wrap(~ idx, scale = "free") +
              theme_bw() + # geom_hline(data = d,aes(yintercept = kp))+
              # geom_hline(yintercept = q)+
              theme(axis.text.x = element_text(angle = 90, size = 8))
          })



          output$figure_prob_RH <- renderPlot({
            estt %>%
              filter(stringr::str_detect(grupos, input$group_est)) %>%
              data.frame() %>%
              ggplot() +
              geom_line(aes(t, Fy_z)) +
              # geom_point(aes(t,value))+
              ylab(expression("Prob(RH>0.70)")) + # scale_y_continuous(labels=scales::percent)+
              # scale_x_discrete(labels = c(month.abb))+
              facet_wrap(~ idx, scale = "free") +
              theme_bw() + # geom_hline(data = d,aes(yintercept = kp))+
              # geom_hline(yintercept = q)+
              theme(axis.text.x = element_text(angle = 90, size = 8))
          })


          gerar.grafico <- eventReactive(input$botao,{
            cidade.aux <- am[stringr::str_detect(cidades, input$city),]
              lat <- cidade.aux[["LATITUDE"]]
              long <- cidade.aux[["LONGITUDE"]]
              par(mfrow=c(1,2))
              plot(cidade.aux)
              plot(am);points(long,lat,cex=2,col=2,pch=18)

          })

           output$grafico_maps <- renderPlot({

             gerar.grafico()

          #   cidade.aux <- am[stringr::str_detect(cidades, input$city),]
          #   lat <- cidade.aux[["LATITUDE"]]
          #   long <- cidade.aux[["LONGITUDE"]]
          #
          #   par(mfrow=c(1,2))
          #   plot(cidade.aux)
          #   plot(am);points(long,lat,cex=2,col=2,pch=18)
          #
           })
        }

        shinyApp(ui, server)


        # .rs.files.restoreBindings() tive que rodar esse código antes de publicar o app
