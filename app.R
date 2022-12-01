library(shiny)
library(tidyverse)
library(plotly)

# Steps after each season
# 1. Check that LL_history.csv has the same format as it did previously
# 2. Add data from the most recent season
# 3. Import updated LL_history.csv file into LL_players app folder
LLhistory <- read.csv("LL_history.csv")
# 4. Check if the LL champion needs to be updated

curr <- max(LLhistory$Season) # current season

# Define UI for application that creates the dashboard
ui <- fluidPage(
   titlePanel(paste0("LearnedLeague performance, LL60 to LL", curr)),
   sidebarLayout(
      sidebarPanel(
            selectizeInput(inputId = "name",
                        label = "Select name:",
                        choices = NULL),
            hr(),
            h6("Values for the selected player are indicated by open red circles."),
            h6("The smaller points show the values for others in the player's rundle.")
      ),
      mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("QPct", plotlyOutput(outputId = "QPct"),
                                 verbatimTextOutput("hover")), 
                        tabPanel("Rundle Rank", plotOutput(outputId = "Rrank")),
                        tabPanel("Rundle Rank (tiered)", plotOutput(outputId = "tieredrank")),
                        tabPanel("DE", plotOutput("DE")),
                        tabPanel("OE", plotOutput("OE"), br(), 
                                 h6(textOutput("caveat"))),
                        tabPanel("MCW", plotOutput("MCW"))
                        )
      )
   )
)

server <- function(input, output, session) {
      updateSelectizeInput(session, inputId = "name",
                           choices = unique(LLhistory$Player),
                           server = TRUE,
                           selected = "FrielP")
      output$QPct <- renderPlotly({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, DE)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qDE")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(60:curr)
            Player <- historyDF$Player
            LLama <- input$name
        p <- ggplot(historyDF, aes(key = Player, x = Season, y = QPct, color = Level)) +
            geom_point(alpha = 0.6, size = 1.5) +
            scale_colour_manual(values = cols) +
            scale_x_discrete(breaks = LLseasons, drop = FALSE) +
            scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
            geom_point(inherit.aes = FALSE, aes(key = LLama, x = Season, y = qQPct), 
                       color = "red", shape = 1, size = 5) +
            labs(x = "Season", y = "QPct") +
            theme_classic()
        ggplotly(p)
   })
      output$Rrank <- renderPlot({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, DE)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qDE")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(60:curr)
         ggplot(historyDF, aes(x = as.factor(Season), y = fct_rev(as.factor(Rundle.Rank)),
                               color = Level, size = QPct)) +
               geom_point(alpha = 0.6) +
               scale_colour_manual(values = cols) +
               scale_x_discrete(breaks = LLseasons, drop = FALSE) +
               geom_point(inherit.aes = FALSE, aes(x = as.factor(Season), 
                  y = fct_rev(as.factor(RRank))), color = "red", shape = 1, size = 7) +
               labs(x = "Season", y = "Rundle Rank") +
               theme_classic()
   })
      output$tieredrank <- renderPlot({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, DE)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qDE")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(60:curr)
            historyDF$overall <- recode(historyDF$Level, "A" = 0, "B" = 32, "C" = 64, "D" = 98,
                                        "E" = 132, "R" = 166)
            historyDF$pos <- historyDF$overall + historyDF$Rundle.Rank
            historyDF$ind <- historyDF$overall + historyDF$RRank
            ggplot(historyDF, aes(x = ind, y = fct_rev(as.factor(Season)),
                                  color = Level)) +
                  geom_point(size = 3) +
                  geom_point(aes(x = pos, y = as.factor(Season)), size = 0.5) +
                  scale_colour_manual(values = cols) +
                  scale_x_reverse(limits = c(200, 0)) +
                  scale_y_discrete(breaks = LLseasons, drop = FALSE) +
                  labs(x = NULL, y = "Season") +
                  theme_classic() +
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.line.x = element_blank())
      })
      output$DE <- renderPlot({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, DE)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qDE")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(60:curr)
            ggplot(historyDF, aes(x = as.factor(Season), y = DE, color = Level)) +
                  geom_point(alpha = 0.6) +
                  scale_colour_manual(values = cols) +
                  scale_x_discrete(breaks = LLseasons, drop = FALSE) +
                  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
                  geom_point(inherit.aes = FALSE, aes(x = as.factor(Season), y = qDE),
                             color = "red", shape = 1, size = 7) +
                  labs(x = "Season", y = "DE") +
                  theme_classic()
      })
      output$OE <- renderPlot({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  filter(Season > 70) %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, OE)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qOE")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(71:curr)
            ggplot(historyDF, aes(x = as.factor(Season), y = OE, color = Level)) +
                  geom_point(alpha = 0.6) +
                  scale_colour_manual(values = cols) +
                  scale_x_discrete(breaks = LLseasons, drop = FALSE) +
                  scale_y_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(n = 6)) +
                  geom_point(inherit.aes = FALSE, aes(x = as.factor(Season), y = qOE),
                             color = "red", shape = 1, size = 7) +
                  labs(x = "Season", y = "OE") +
                  theme_classic()
      })
      output$caveat <- renderText({
            "OE data are available only for LL71 onward"
            })
      output$MCW <- renderPlot({
            playerDF <- LLhistory[LLhistory$Player == input$name, ] %>%
                  filter(!is.na(MCW)) %>%
                  select(Player, Season, Level, Rundle, QPct, Rundle.Rank, MCW)
            colnames(playerDF) <- c("qPlayer", "Season", "Level", "Rundle", "qQPct", "RRank", "qMCW")
            historyDF <- left_join(playerDF, LLhistory, by = c("Season", "Level", "Rundle"))
            cols <- c("A" = "#66c2a5", "B" = "#fc8d62", "C" = "#8da0cb", "D" = "#e78ac3",
                      "E" = "#a6d854", "R" = "#ffd92f")
            LLseasons <- as.character(60:curr)
            ggplot(historyDF, aes(x = as.factor(Season), y = MCW, color = Level)) +
                  geom_point(alpha = 0.6) +
                  scale_colour_manual(values = cols) +
                  scale_x_discrete(breaks = LLseasons, drop = FALSE) +
                  scale_y_continuous(limits = c(0, 50), breaks = scales::pretty_breaks(n = 6)) +
                  geom_point(inherit.aes = FALSE, aes(x = as.factor(Season), y = qMCW),
                             color = "red", shape = 1, size = 7) +
                  labs(x = "Season", y = "MCWA") +
                  theme_classic()
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

