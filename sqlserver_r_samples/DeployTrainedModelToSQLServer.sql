---create a table to stored the serialized model in SQL Server
IF OBJECT_ID('wwi_dw_trained_SameDayFF_model') IS NOT NULL
	DROP TABLE wwi_dw_trained_SameDayFF_model

GO

CREATE TABLE wwi_dw_trained_SameDayFF_model
(
	model varbinary(max) not null
);

GO

--this stored procedure defines the training data set
--and model that will be trained and stored as a
--binary object in SQL Server
CREATE OR ALTER PROCEDURE [dbo].[TrainSameDayFullfillmentPredictionModel]  

AS  
BEGIN  
  --this query defines the training data set
  DECLARE @inquery nvarchar(max) = N'  
    SELECT 
	  CASE WHEN [Order Date Key] = [Picked Date Key] THEN 1 ELSE 0 END AS SameDayFulfillment,
	  [City Key] AS city,
	  [Stock Item Key] AS item,
	  [Picker Key] AS picker,
	  [Quantity] AS quantity
	FROM [Fact].[Order]
	TABLESAMPLE (75 PERCENT)
	WHERE YEAR([Order Date Key]) * 10000 + MONTH([Order Date Key]) * 100 + DAY([Order Date Key]) <= 20151231 
	'  
  --this function executes an R script using the in-Database MRS engine  
  --serializes the trained model as an object and inserts into the 
  --table created in the first step above
  INSERT INTO wwi_dw_trained_SameDayFF_model  
  EXEC sp_execute_external_script 
    @language = N'R',  
    @script = N'  
		## Create model  
		logitObj <- rxLogit(SameDayFulfillment ~ city + item + picker + quantity, data = InputDataSet)  
		summary(logitObj)  
		str(logitObj)

		## Serialize model and put it in data frame  
		trained_SameDayFullfillment_model <- data.frame(model=as.raw(serialize(logitObj, NULL)));

		',  
    @input_data_1 = @inquery,  
    @output_data_1_name = N'trained_SameDayFullfillment_model'  
  ;  

END  
GO  

--execute the stored procedure to create the trained model
EXEC TrainSameDayFullfillmentPredictionModel;

--verify the model is in the table
SELECT * FROM wwi_dw_trained_SameDayFF_model;

