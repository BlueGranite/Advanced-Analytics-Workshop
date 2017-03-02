--this stored procedure deserializes the model back in R
--and accepts a query than defines a different set of data for scoring.
CREATE OR ALTER PROCEDURE [dbo].[PredictSameDayFullfillment] @inquery nvarchar(max)  
AS  
BEGIN  

  DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM wwi_dw_trained_SameDayFF_model);  
  
  EXEC sp_execute_external_script 
	@language = N'R',  
    @script = N'  
				mod <- unserialize(as.raw(model));  
				print(summary(mod))  
				OutputDataSet<-rxPredict(modelObject = mod, data = InputDataSet, outData = NULL,   
						  predVarNames = "Score", type = "response", writeModelVars = TRUE, overwrite = TRUE);  
				str(OutputDataSet)  
				print(OutputDataSet)  
				',  
	@input_data_1 = @inquery,  
	@params = N'@model varbinary(max)',  
	@model = @lmodel2  
  WITH RESULT SETS ((Score float, SameDayFullfillment int, city int, item int, picker int, quantity int));  
  
END  
   
GO

DECLARE @DataSetToScore nvarchar(max) = N'  
    SELECT 
  CASE WHEN [Order Date Key] = [Picked Date Key] THEN 1 ELSE 0 END AS SameDayFulfillment,
  [City Key] AS city,
  [Stock Item Key] AS item,
  [Picker Key] AS picker,
  [Quantity] AS quantity
FROM [Fact].[Order]
WHERE YEAR([Order Date Key]) * 10000 + MONTH([Order Date Key]) * 100 + DAY([Order Date Key]) >= 20160101'


EXEC PredictSameDayFullfillment @DataSetToScore; 