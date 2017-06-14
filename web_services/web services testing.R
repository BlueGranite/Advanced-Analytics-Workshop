
##             MODEL DEPLOYMENT EXAMPLE                 ##

## enhanced by BlueGranite, Inc

## script contents from
## https://msdn.microsoft.com/en-us/microsoft-r/operationalize/data-scientist-get-started#share
## https://msdn.microsoft.com/en-us/microsoft-r/operationalize/data-scientist-manage-services

##########################################################
#         Load mrsdeploy package on R Server             #
##########################################################

library(mrsdeploy)
library(tidyverse)

##########################################################
#            Log into Microsoft R Server                 #
##########################################################

# Use `remoteLogin` to authenticate with R Server using 
# the local admin account. Use session = false so no 
# remote R session started
remoteLogin("http://localhost:12800", 
            username = "admin", 
            #password = "{{YOUR_PASSWORD}}",
            password = "BlueGranite1!",
            session = FALSE)

# use the listServices() function from 'mrsdeploy' to return current web services info
listServices() # all services
listServices("housepricePredictService1482282060") # a specific service

# table of current published web services - name, version #, and owner
my_web_services <- data.frame(name    = listServices() %>% map_chr("name"),
                              version = listServices() %>% map_chr("version"),
                              owner   = listServices() %>% map_chr("owner"))
my_web_services

 ##########################################################
#       Create & Test a Logistic Regression Model        #
##########################################################

# Use logistic regression equation of vehicle transmission 
# in the data set mtcars to estimate the probability of 
# a vehicle being fitted with a manual transmission 
# based on horsepower (hp) and weight (wt)


# Create glm model with `mtcars` dataset
carsModel <- glm(formula = am ~ hp + wt, data = mtcars, family = binomial)

# Produce a prediction function that can use the model
manualTransmission <- function(hp, wt) {
  newdata <- data.frame(hp = hp, wt = wt)
  predict(carsModel, newdata, type = "response")
}

# test function locally by printing results
print(manualTransmission(120, 2.8)) # 0.6418125



##########################################################
#             Publish Model as a Service                 #
##########################################################

# Publish as service using `publishService()` function from 
# `mrsdeploy` package. Name service "mtService" and provide
# unique version number. Assign service to the variable `api`
api <- publishService(
  "mtService",
  code = manualTransmission,
  model = carsModel,
  inputs = list(hp = "numeric", wt = "numeric"),
  outputs = list(answer = "numeric"),
  v = "v1.0.0"
)

# if the service already exists, use this
api <- getService("mtService", "v1.0.0")

##########################################################
#                 Consume Service in R                   #
##########################################################

# Print capabilities that define the service holdings: service 
# name, version, descriptions, inputs, outputs, and the 
# name of the function to be consumed
print(api$capabilities())
print(api)

# Print the service name, version, inputs, outputs, and the
# Swagger-based JSON file used to consume the service 
cap <- api$capabilities()
print(cap$name)
print(cap$version)
print(cap$inputs)
print(cap$outputs)
print(cap$swagger)

# Start interacting with the service by calling it with the
# generic name `consume` based on I/O schema
result <- api$consume(120, 2.8)

# Or, start interacting with the service using the alias argument
# that was defined at publication time.
result <- api$manualTransmission(120, 2.8)

# Print response output named `answer`
print(result$output("answer")) # 0.6418125   

##########################################################
#         Get Service-specific Swagger File in R         #
##########################################################

# During this authenticated session, download the  
# Swagger-based JSON file that defines this service
swagger <- api$swagger()
cat(swagger, file = "swagger.json", append = FALSE)

# Share Swagger-based JSON with those who need to consume it