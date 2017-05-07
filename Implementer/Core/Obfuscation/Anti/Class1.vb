Imports System.Runtime.InteropServices

Namespace Injections
    Public Class AntiDumping
        <DllImportAttribute("kernel32.dll")>
        Private Shared Function VirtualProtect(lpAddress As IntPtr, dwSize As Integer, flNewProtect As UInteger, ByRef lpflOldProtect As UInteger) As Boolean
        End Function

        Public Shared Sub Initialize()
            Dim old As UInteger
            Dim bas As IntPtr = CType(Marshal.GetHINSTANCE(GetType(AntiDumping).[Module]), IntPtr)
            Dim ptr As IntPtr = bas + &H3C
            Dim ptr2 As IntPtr
            ptr = InlineAssignHelper(ptr2, bas + ptr)
            ptr += &H6
            Dim sectNum As UShort = ptr
            ptr += 14
            Dim optSize As UShort = ptr
            ptr = InlineAssignHelper(ptr2, ptr + &H4 + optSize)

            Dim [new] As IntPtr = stackalloc
            ' (byte*)Marshal.AllocHGlobal(11);
            If GetType(AntiDumping).[Module].FullyQualifiedName(0) <> "<"c Then
                'Mapped
                'VirtualProtect(ptr - 16, 8, 0x40, out old);
                '*(uint*)(ptr - 12) = 0;
                Dim mdDir As Pointer(Of Byte) = bas + CType(ptr - 16, Pointer(Of UInteger)).Target
                '*(uint*)(ptr - 16) = 0;

                If CType(ptr - &H78, Pointer(Of UInteger)).Target <> 0 Then
                    Dim importDir As Pointer(Of Byte) = bas + CType(ptr - &H78, Pointer(Of UInteger)).Target
                    Dim oftMod As Pointer(Of Byte) = bas + CType(importDir, Pointer(Of UInteger)).Target
                    Dim modName As Pointer(Of Byte) = bas + CType(importDir + 12, Pointer(Of UInteger)).Target
                    Dim funcName As Pointer(Of Byte) = bas + CType(oftMod, Pointer(Of UInteger)).Target + 2
                    VirtualProtect(modName, 11, &H40, old)

                    CType([new], Pointer(Of UInteger)).Target = &H6C64746E
					(CType([new], Pointer(Of UInteger)) + 1).Target = &H6c642e6c
					(CType([new], Pointer(Of UShort)) + 4).Target = &H6c
					([new] + 10).Target = 0

					For i As Integer = 0 To 10
						(modName + i).Target = ([new] + i).Target
					Next

                    VirtualProtect(funcName, 11, &H40, old)

                    CType([new], Pointer(Of UInteger)).Target = &H6F43744E
					(CType([new], Pointer(Of UInteger)) + 1).Target = &H6e69746e
					(CType([new], Pointer(Of UShort)) + 4).Target = &H6575
					([new] + 10).Target = 0

					For i As Integer = 0 To 10
						(funcName + i).Target = ([new] + i).Target
					Next
                End If

                For i As Integer = 0 To sectNum - 1
                    VirtualProtect(ptr, 8, &H40, old)
                    Marshal.Copy(New Byte(7) {}, 0, DirectCast(ptr, IntPtr), 8)
                    ptr += &H28
                Next
                VirtualProtect(mdDir, &H48, &H40, old)
                Dim mdHdr As Pointer(Of Byte) = bas + CType(mdDir + 8, Pointer(Of UInteger)).Target
                CType(mdDir, Pointer(Of UInteger)).Target = 0
				(CType(mdDir, Pointer(Of UInteger)) + 1).Target = 0
				(CType(mdDir, Pointer(Of UInteger)) + 2).Target = 0
				(CType(mdDir, Pointer(Of UInteger)) + 3).Target = 0

				VirtualProtect(mdHdr, 4, &H40, old)
                CType(mdHdr, Pointer(Of UInteger)).Target = 0
                mdHdr += 12
                mdHdr += CType(mdHdr, Pointer(Of UInteger)).Target
                mdHdr = CType((CUInt(mdHdr) + 7) And Not 3, Pointer(Of Byte))
                mdHdr += 2
                Dim numOfStream As UShort = mdHdr.Target
                mdHdr += 2
                For i As Integer = 0 To numOfStream - 1
                    VirtualProtect(mdHdr, 8, &H40, old)
                    '*(uint*)mdHdr = 0;
                    mdHdr += 4
                    '*(uint*)mdHdr = 0;
                    mdHdr += 4
                    For ii As Integer = 0 To 7
                        VirtualProtect(mdHdr, 4, &H40, old)
                        mdHdr.Target = 0
                        mdHdr += 1
                        If mdHdr.Target = 0 Then
                            mdHdr += 3
                            Exit For
                        End If
                        mdHdr.Target = 0
                        mdHdr += 1
                        If mdHdr.Target = 0 Then
                            mdHdr += 2
                            Exit For
                        End If
                        mdHdr.Target = 0
                        mdHdr += 1
                        If mdHdr.Target = 0 Then
                            mdHdr += 1
                            Exit For
                        End If
                        mdHdr.Target = 0
                        mdHdr += 1
                    Next
                Next
            Else
                'Flat
                'VirtualProtect(ptr - 16, 8, 0x40, out old);
                '*(uint*)(ptr - 12) = 0;
                Dim mdDir As UInteger = CType(ptr - 16, IntPtr)
                '*(uint*)(ptr - 16) = 0;
                Dim importDir As UInteger = CType(ptr - &H78, IntPtr)

                Dim vAdrs As UInteger() = New UInteger(sectNum - 1) {}
                Dim vSizes As UInteger() = New UInteger(sectNum - 1) {}
                Dim rAdrs As UInteger() = New UInteger(sectNum - 1) {}
                For i As Integer = 0 To sectNum - 1
                    VirtualProtect(ptr, 8, &H40, old)
                    Marshal.Copy(New Byte(7) {}, 0, DirectCast(ptr, IntPtr), 8)
                    vAdrs(i) = CType(ptr + 12, IntPtr).Target
                    vSizes(i) = CType(ptr + 8, IntPtr).Target
                    rAdrs(i) = CType(ptr + 20, IntPtr).Target
                    ptr += &H28
                Next


                If importDir <> 0 Then
                    For i As Integer = 0 To sectNum - 1
                        If vAdrs(i) < importDir AndAlso importDir < vAdrs(i) + vSizes(i) Then
                            importDir = importDir - vAdrs(i) + rAdrs(i)
                            Exit For
                        End If
                    Next
                    Dim importDirPtr As IntPtr = bas + importDir
                    Dim oftMod As UInteger = importDirPtr
                    For i As Integer = 0 To sectNum - 1
                        If vAdrs(i) < oftMod AndAlso oftMod < vAdrs(i) + vSizes(i) Then
                            oftMod = oftMod - vAdrs(i) + rAdrs(i)
                            Exit For
                        End If
                    Next
                    Dim oftModPtr As IntPtr = bas + oftMod
                    Dim modName As UInteger = CType(importDirPtr + 12, IntPtr)
                    For i As Integer = 0 To sectNum - 1
                        If vAdrs(i) < modName AndAlso modName < vAdrs(i) + vSizes(i) Then
                            modName = modName - vAdrs(i) + rAdrs(i)
                            Exit For
                        End If
                    Next
                    Dim funcName As UInteger = CType(oftModPtr, Pointer(Of UInteger)).Target + 2
                    For i As Integer = 0 To sectNum - 1
                        If vAdrs(i) < funcName AndAlso funcName < vAdrs(i) + vSizes(i) Then
                            funcName = funcName - vAdrs(i) + rAdrs(i)
                            Exit For
                        End If
                    Next
                    VirtualProtect(bas + modName, 11, &H40, old)

                    CType([new], Pointer(Of UInteger)).Target = &H6C64746E
					(CType([new], Pointer(Of UInteger)) + 1).Target = &H6c642e6c
					(CType([new], Pointer(Of UShort)) + 4).Target = &H6c
					([new] + 10).Target = 0

					For i As Integer = 0 To 10
						(bas + modName + i).Target = ([new] + i).Target
					Next

                    VirtualProtect(bas + funcName, 11, &H40, old)

                    CType([new], Pointer(Of UInteger)).Target = &H6F43744E
					(CType([new], Pointer(Of UInteger)) + 1).Target = &H6e69746e
					(CType([new], Pointer(Of UShort)) + 4).Target = &H6575
					([new] + 10).Target = 0

					For i As Integer = 0 To 10
						(bas + funcName + i).Target = ([new] + i).Target
					Next
                End If


                For i As Integer = 0 To sectNum - 1
                    If vAdrs(i) < mdDir AndAlso mdDir < vAdrs(i) + vSizes(i) Then
                        mdDir = mdDir - vAdrs(i) + rAdrs(i)
                        Exit For
                    End If
                Next
                Dim mdDirPtr As Pointer(Of Byte) = bas + mdDir
                VirtualProtect(mdDirPtr, &H48, &H40, old)
                Dim mdHdr As UInteger = CType(mdDirPtr + 8, Pointer(Of UInteger)).Target
                For i As Integer = 0 To sectNum - 1
                    If vAdrs(i) < mdHdr AndAlso mdHdr < vAdrs(i) + vSizes(i) Then
                        mdHdr = mdHdr - vAdrs(i) + rAdrs(i)
                        Exit For
                    End If
                Next
                CType(mdDirPtr, Pointer(Of UInteger)).Target = 0
				(CType(mdDirPtr, Pointer(Of UInteger)) + 1).Target = 0
				(CType(mdDirPtr, Pointer(Of UInteger)) + 2).Target = 0
				(CType(mdDirPtr, Pointer(Of UInteger)) + 3).Target = 0


				Dim mdHdrPtr As Pointer(Of Byte) = bas + mdHdr
                VirtualProtect(mdHdrPtr, 4, &H40, old)
                CType(mdHdrPtr, Pointer(Of UInteger)).Target = 0
                mdHdrPtr += 12
                mdHdrPtr += CType(mdHdrPtr, Pointer(Of UInteger)).Target
                mdHdrPtr = CType((CUInt(mdHdrPtr) + 7) And Not 3, Pointer(Of Byte))
                mdHdrPtr += 2
                Dim numOfStream As UShort = mdHdrPtr.Target
                mdHdrPtr += 2
                For i As Integer = 0 To numOfStream - 1
                    VirtualProtect(mdHdrPtr, 8, &H40, old)
                    '*(uint*)mdHdrPtr = 0;
                    mdHdrPtr += 4
                    '*(uint*)mdHdrPtr = 0;
                    mdHdrPtr += 4
                    For ii As Integer = 0 To 7
                        VirtualProtect(mdHdrPtr, 4, &H40, old)
                        mdHdrPtr.Target = 0
                        mdHdrPtr += 1
                        If mdHdrPtr.Target = 0 Then
                            mdHdrPtr += 3
                            Exit For
                        End If
                        mdHdrPtr.Target = 0
                        mdHdrPtr += 1
                        If mdHdrPtr.Target = 0 Then
                            mdHdrPtr += 2
                            Exit For
                        End If
                        mdHdrPtr.Target = 0
                        mdHdrPtr += 1
                        If mdHdrPtr.Target = 0 Then
                            mdHdrPtr += 1
                            Exit For
                        End If
                        mdHdrPtr.Target = 0
                        mdHdrPtr += 1
                    Next
                Next
            End If
            'Marshal.FreeHGlobal((IntPtr)@new);
        End Sub
        Private Shared Function InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
            target = value
            Return value
        End Function
    End Class
End Namespace
