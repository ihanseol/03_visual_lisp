Sub InsertDXFFiles()
    ' Variables declaration
    Dim downloadsFolder As String
    Dim dxfFile As String
    Dim fileIndex As Integer
    Dim fileCount As Integer
    Dim insertPoint(0 To 2) As Double
    Dim scale As Double
    Dim rotation As Double
    
    ' Set Downloads folder path (Update the path according to your system)
    downloadsFolder = "C:\Users\minhwasoo\Downloads\"
    
    ' Initialize insertion point, scale, and rotation
    insertPoint(0) = 0: insertPoint(1) = 0: insertPoint(2) = 0
    scale = 1
    rotation = 0
    
    ' Get the first DXF file
    dxfFile = Dir(downloadsFolder & "*.dxf")
    
    ' Initialize file counter
    fileCount = 0
    
    ' Loop through the files and insert up to 9 DXF files
    Do While dxfFile <> "" And fileCount < 9
        ' Insert DXF file
        ThisDrawing.ModelSpace.InsertBlock insertPoint, downloadsFolder & dxfFile, scale, scale, scale, rotation
        
        ' Move to the next DXF file
        dxfFile = Dir
        fileCount = fileCount + 1
    Loop
    
    ' Notify user if no DXF files are found
    If fileCount = 0 Then
        MsgBox "No DXF files found in the Downloads folder."
    End If
End Sub
