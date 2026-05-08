
Shiny App for robustMDAS
library(shiny)
library(robustMDAS)
library(ggplot2)

Define UI
ui <- fluidPage(
titlePanel("Robust MDAS Diagnostic Tool"),
sidebarLayout(
sidebarPanel(
fileInput("file", "Upload Clinical Data (CSV)",
accept = c(".csv")),
numericInput("n_replications", "Monte Carlo Replications",
value = 100, min = 50, max = 500),
actionButton("run", "Run Diagnostic", class = "btn-primary")
),
mainPanel(
tabsetPanel(
tabPanel("Results", verbatimTextOutput("results")),
tabPanel("Visualization", plotOutput("plot")),
tabPanel("Help", htmlOutput("help"))
)
)
)
)

Define server
server <- function(input, output) {

observeEvent(inputrun, { req(inputfile)

data <- read.csv(input
f
i
l
e
filedatapath)

Preprocess (assume last column is outcome)
outcome_col <- names(data)[ncol(data)]
feature_cols <- names(data)[1:(ncol(data)-1)]

X <- as.matrix(data[, feature_cols])

Split
set.seed(123)
n_total <- nrow(X)
n_train <- floor(0.8 * n_total)
train_idx <- sample(n_total, n_train)

X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]

Run diagnostic
result <- robust_diagnostic(X_train, X_test,
n_replications = input$n_replications,
verbose = FALSE)

outputresults <- renderPrint({ cat("Combined p-value:", resultp_combined, "\n")
cat("Decision:", ifelse(resultreject_null, "INADEQUATE", "ADEQUATE"), "\n") cat("\nIndividual p-values:\n") print(round(resultp_values, 4))
if (resultreject_null) { cat("\nProblematic feature:", resultks_diagnosis$feature_max, "\n")
}
})

if (!is.null(resultreference_distribution)) { outputplot <- renderPlot({
hist(result
r
e
f
e
r
e
n
c
e
d
i
s
t
r
i
b
u
t
i
o
n
reference 
d
​
 istributionmdas,
breaks = 30,
col = "steelblue",
border = "white",
main = "Reference Distribution",
xlab = "Robust MDAS")
abline(v = result
o
b
s
e
r
v
e
d
s
t
a
t
s
observed 
s
​
 tatsmdas, col = "red", lwd = 2, lty = 2)
})
}
})

output$help <- renderUI({
HTML("

<h3>Instructions</h3> <ol> <li>Upload a CSV file with clinical data</li> <li>The last column should be the outcome variable</li> <li>All other columns will be used as features</li> <li>Click 'Run Diagnostic' to assess split quality</li> </ol> <h3>Interpretation</h3> <ul> <li><b>p-value &lt; 0.05</b>: Split is INADEQUATE</li> <li><b>p-value &gt; 0.05</b>: Split is ADEQUATE</li> </ul> ") }) }
Run the app
shinyApp(ui = ui, server = server)
