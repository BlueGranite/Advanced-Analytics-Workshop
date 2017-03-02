CREATE OR ALTER PROCEDURE [PredictSingleDataFullfillment] 
    @inSameDayFullfillment int = 0,
	@inCity int = 0, 
	@inItem int = 0, 
	@inPicker int = 0, 
	@inQuantity int = 0
AS
BEGIN
	DECLARE @inquery nvarchar(max) = N'
	SELECT ' + CAST(@inSameDayFullfillment AS VARCHAR) + ' as SameDayFullfillment, 
	       ' + CAST(@inCity AS VARCHAR) + ' as city, 
		   ' + CAST(@inItem AS VARCHAR)+ ' as item, 
		   ' + CAST(@inPicker AS VARCHAR) + ' as picker, 
		   ' + CAST(@inQuantity AS VARCHAR) + ' as quantity'

	PRINT @inquery
	DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM wwi_dw_trained_SameDayFF_model);  
  
	EXEC sp_execute_external_script @language = N'R',  
                                  @script = N'  
	mod <- unserialize(as.raw(model));  
	print(summary(mod))  
	OutputDataSet<-rxPredict(modelObject = mod, data = InputDataSet, outData = NULL,   
			  predVarNames = "Score", type = "response", writeModelVars = FALSE, overwrite = TRUE);  
	str(OutputDataSet)  
	print(OutputDataSet)  
	',  
	@input_data_1 = @inquery,  
	@params = N'@model varbinary(max), @SameDayFullfillment int, 
				@city int, @item int, @picker int, @quantity int',	    
	@model = @lmodel2,
	@SameDayFullfillment = @inSameDayFullfillment,
	@city = @inCity,
	@item = @inItem,
	@picker = @inPicker,
	@quantity = @inQuantity  
	WITH RESULT SETS ((Score float));

END
GO

EXEC PredictSingleDataFullfillment 1, 107255, 197 , 145, 17