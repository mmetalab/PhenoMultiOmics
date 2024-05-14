
R Shiny is a framework for the R programming language, used to build interactive web applications, consisting of two main parts: the User Interface (UI) and the Server logic. The phenomultiomics website contains a main app.R file, which, when run, operates by calling 4 server files and 6 UI files.


Open the app.R file and click Run App. Follow the prompts to install the required R packages. We are using the Shinyapps.io platform for deployment. After registering and logging into Shinyapps.io, select an appropriate service plan. Then, obtain the API key from your Shinyapps.io account and set up these credentials in RStudio. This allows for direct deployment of the application from RStudio. Use the rsconnect::deployApp() command to upload the application to the platform.

Once the deployment is complete, visit the URL provided by Shinyapps.io to verify that the application is running properly. Conduct thorough functional tests and performance evaluations to ensure that the application meets expectations when running on the server. Also, use the monitoring tools provided by Shinyapps.io to track the application's operational status and usage, and regularly review logs to promptly identify and resolve potential issues.
