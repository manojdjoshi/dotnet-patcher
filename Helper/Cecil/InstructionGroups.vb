Imports System.Runtime.InteropServices

Namespace CecilHelper
    Public Class InstructionGroups
        Inherits List(Of InstructionGroup)

        Private Shared r As Random = New Random

        Public Function getGroup(ByVal id As Integer) As InstructionGroup
            Dim group As InstructionGroup
            For Each group In Me
                If (group.ID = id) Then
                    Return group
                End If
            Next
            Throw New Exception("Invalid ID!")
        End Function

        Public Function getLast() As InstructionGroup
            Return Me.getGroup((MyBase.Count - 1))
        End Function

        Public Sub Scramble(<Out> ByRef incGroups As InstructionGroups)
            Dim groups As New InstructionGroups
            Dim group As InstructionGroup
            For Each group In Me
                groups.Insert(r.Next(0, groups.Count), group)
            Next
            incGroups = groups
        End Sub

    End Class
End Namespace
