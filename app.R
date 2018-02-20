# Vim wacked out my spacing - sorry!
library(shiny)
library(readr)
library(DT)
library(shinydashboard)
library(googleAuthR)
library(RoogleVision)
library(shinycssloaders)

# Authenticating to Google
# Have to get credentials for Google Developer Console

CLIENT_ID  <- ""
CLIENT_SECRET  <- ""
PATH_TO_SERVICE  <- ""

options("googleAuthR.webapp.client_id" = CLIENT_ID)
options("googleAuthR.webapp.client_secret" = CLIENT_SECRET)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))

service_token  <- gar_auth_service(json_file = PATH_TO_SERVICE)


header  <- dashboardHeader(
	title = "PICASSO",
	titleWidth = 300
)

body  <- dashboardBody(

	fluidRow(

		column(width = 6,

			box(width = NULL,
			title = "Input Image",
			status = "danger",
			solidHeader = FALSE,
				imageOutput('image') %>% withSpinner()
			)
			
		),

		column(width = 6,

			box(width = NULL,
			title = "Google Cloud Vision API",
			status = "danger",
			p("The model takes an image and returns a dataframe of attributes for the given method."),
			selectInput('method', 'Detection Method:',
									c('Label Detection' = 'LABEL_DETECTION',
										# TODO: Figuring out what data belongs to which face
										'Face Detection' = 'FACE_DETECTION',
										'Logo Detection' = 'LOGO_DETECTION',
										'Text Detection' = 'TEXT_DETECTION',
										'Landmark Detection' = 'LANDMARK_DETECTION')),
			fileInput('image1', 'Select Image:',
				accept = c(".png", ".jpg", ".jpeg"))))
		),

		column(width = 12,

			box(width = NULL,
			solidHeader = FALSE,
			status = "danger",
				DT::dataTableOutput('image_data') %>% withSpinner()
			)

		)

	)
)

ui  <- dashboardPage(
	skin = "red",
	header,
	dashboardSidebar(disable = TRUE),
	body
)

server <- function(input, output) { 

	detection_method  <- reactive({
		#	Allow users to change detection method
		input$method
	})

	output$image  <- renderImage({
		# Display default image, or use user supplied one
		if (is.null(input$image1)) {
			outfile  <- 'picasso_bull.jpeg'	
			content_type  <- 'image/jpg'
		} else {
			outfile  <- input$image1$datapath	
			content_type  <- input$image1$type
		}

		# Constrain image output size, so images don't outgrow box
		list(src = outfile, contentType = content_type, width = 500, height = 400)
	}, deleteFile = FALSE)


	output$image_data  <- DT::renderDataTable({
	
		if (is.null(input$image1)) {
		  # Default DF to read in
			df  <- readr::read_csv('bull_data.csv')		
			return(DT::datatable(df))
		} else {

			if (detection_method() == "FACE_DETECTION") {
			# Face Detection returns A LOT of information
			# Filtering final DF for key metrics of interest
			response  <- getGoogleVisionResponse(input$image1$datapath, feature = "FACE_DETECTION", numResults = 20)

			df_res  <- as.data.frame(response)
			# Filter DF for columns of interest
			df_res  <- df_res[, c("joyLikelihood", "sorrowLikelihood",
														"angerLikelihood", "surpriseLikelihood",
														"underExposedLikelihood", "blurredLikelihood",
														"headwearLikelihood")]

			return(DT::datatable(df_res, extensions = 'Buttons',
						options = list(dom = 'Bfrtip', buttons = c('csv', 'excel'))))

			} else {

			response  <- getGoogleVisionResponse(input$image1$datapath, feature = detection_method(), numResults = 20)

			df_res  <- as.data.frame(response)

			return(DT::datatable(df_res, extensions = 'Buttons',
						options = list(dom = 'Bfrtip', buttons = c('csv', 'excel'))))
			}
		
		}
	
	})


}

shinyApp(ui, server)
