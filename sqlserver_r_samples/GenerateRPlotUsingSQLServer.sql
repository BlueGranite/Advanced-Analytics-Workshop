--CREATE OR ALTER PROCEDURE [dbo].[PlotHistogram]  
--AS  
--BEGIN  
--  SET NOCOUNT ON;  
  DECLARE @query nvarchar(max) =  
  N'SELECT CASE WHEN [Order Date Key] = [Picked Date Key] THEN 1 ELSE 0 END AS SameDayFullfillment
    FROM [Fact].[Order]'  
  EXECUTE sp_execute_external_script @language = N'R',  
                                     @script = N'  
   image_file = tempfile();  
   jpeg(filename = image_file);  
   #Plot histogram  
   rxHistogram(~SameDayFullfillment, data=InputDataSet, 
               col=''lightgreen'', title = ''Fullfillment Histogram'', 
			   xlab =''Fullfilled Same Day or not'', ylab =''Counts'');  
   dev.off();  
   OutputDataSet <- data.frame(data=readBin(file(image_file, "rb"), what=raw(), n=1e6));  
   ',  
   @input_data_1 = @query  
   WITH RESULT SETS ((plot varbinary(max)));  
--END  

--EXEC [dbo].[PlotHistogram] 
--GO  