// ==++==
//
//   Copyright (c) Microsoft Corporation.  All rights reserved.
//
// ==--==
/*============================================================
**
** Class:  BitConverter
**
**
** Purpose: Allows developers to view the base data types as
**          an arbitrary array of bits.
**
**
===========================================================*/
namespace FsControl.BaseLib
{

    using System;
    using System.Runtime.CompilerServices;
    // The BitConverter class contains methods for
    // converting an array of bytes to one of the base data
    // types, as well as for converting a base data type to an
    // array of bytes.
    //
    // Only statics, does not need to be marked with the serializable attribute
    public static class BitConverter
    {

        // This field indicates the "endianess" of the architecture.
        // The value is set to true if the architecture is
        // little endian; false if it is big endian.
//#if BIGENDIAN
//        public static readonly bool IsLittleEndian /* = false */;
//#else
//        public static readonly bool IsLittleEndian = true;
//#endif

        // Converts a byte into an array of bytes with length one.
        public static byte[] GetBytes(bool value)
        {
            byte[] r = new byte[1];
            r[0] = (value ? (byte) 1 : (byte) 0);
            return r;
        }

        // Converts a char into an array of bytes with length two.
        public static byte[] GetBytes(char value, bool isLittleEndian)
        {
            return GetBytes((short)value, isLittleEndian);
        }

        // Converts a short into an array of bytes with length
        // two.
        public unsafe static byte[] GetBytes(short value, bool isLittleEndian)
        {
            if (!isLittleEndian) return new [] {(byte)(value >> 8), (byte)value};
            byte[] bytes = new byte[2];
            fixed (byte* b = bytes)
                *((short*)b) = value;
            return bytes;
        }

        // Converts an int into an array of bytes with length
        // four.
        public unsafe static byte[] GetBytes(int value, bool isLittleEndian)
        {
            if (!isLittleEndian) return new [] {(byte)(value >> 24), (byte)(value >> 16), (byte)(value >> 8), (byte)value};
            byte[] bytes = new byte[4];
            fixed (byte* b = bytes)
                *((int*)b) = value;
            return bytes;
        }

        // Converts a long into an array of bytes with length
        // eight.
        public unsafe static byte[] GetBytes(long value, bool isLittleEndian)
        {
            if (!isLittleEndian) return new [] {(byte)(value >> 56), (byte)(value >> 48), (byte)(value >> 40), (byte)(value >> 32), (byte)(value >> 24), (byte)(value >> 16), (byte)(value >> 8), (byte)value};
            byte[] bytes = new byte[8];
            fixed (byte* b = bytes)
                *((long*)b) = value;
            return bytes;
        }

        // Converts an ushort into an array of bytes with
        // length two.
        [CLSCompliant(false)]
        public static byte[] GetBytes(ushort value, bool isLittleEndian)
        {
            return GetBytes((short)value, isLittleEndian);
        }

        // Converts an uint into an array of bytes with
        // length four.
        [CLSCompliant(false)]
        public static byte[] GetBytes(uint value, bool isLittleEndian)
        {
            return GetBytes((int)value, isLittleEndian);
        }

        // Converts an unsigned long into an array of bytes with
        // length eight.
        [CLSCompliant(false)]
        public static byte[] GetBytes(ulong value, bool isLittleEndian)
        {
            return GetBytes((long)value, isLittleEndian);
        }

        // Converts a float into an array of bytes with length
        // four.
        public unsafe static byte[] GetBytes(float value, bool isLittleEndian)
        {
            return GetBytes(*(int*)&value, isLittleEndian);
        }

        // Converts a double into an array of bytes with length
        // eight.
        public unsafe static byte[] GetBytes(double value, bool isLittleEndian)
        {
            return GetBytes(*(long*)&value, isLittleEndian);
        }

        // Converts an array of bytes into a char.
        public static char ToChar(byte[] value, int startIndex, bool isLittleEndian)
        {
            return (char)ToInt16(value, startIndex, isLittleEndian);
        }

        // Converts an array of bytes into a short.
        public static unsafe short ToInt16(byte[] value, int startIndex, bool isLittleEndian)
        {
            if (value == null)
                throw new ArgumentNullException("value");

            if ((uint)startIndex >= value.Length)
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_Index");

            if (startIndex > value.Length - 2)
                throw new ArgumentException("Arg_ArrayPlusOffTooSmall");

            fixed (byte* pbyte = &value[startIndex])
            {
                if (isLittleEndian)
                {
                    if (startIndex % 2 == 0) // data is aligned
                        return *((short*)pbyte);

                    return (short)((*pbyte) | (*(pbyte + 1) << 8));
                }
                else
                    return (short)((*pbyte << 8) | (*(pbyte + 1)));
            }
        }

        // Converts an array of bytes into an int.
        public static unsafe int ToInt32(byte[] value, int startIndex, bool isLittleEndian)
        {
            if (value == null)
                throw new ArgumentNullException("value");

            if ((uint)startIndex >= value.Length)
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_Index");

            if (startIndex > value.Length - 4)
                throw new ArgumentException("Arg_ArrayPlusOffTooSmall");

            fixed (byte* pbyte = &value[startIndex])
            {
                if (isLittleEndian)
                {
                    if (startIndex % 4 == 0) // data is aligned
                        return *((int*)pbyte);

                    return (*pbyte) | (*(pbyte + 1) << 8) | (*(pbyte + 2) << 16) | (*(pbyte + 3) << 24);
                }
                else
                    return (*pbyte << 24) | (*(pbyte + 1) << 16) | (*(pbyte + 2) << 8) | (*(pbyte + 3));
            }
        }

        // Converts an array of bytes into a long.
        public static unsafe long ToInt64(byte[] value, int startIndex, bool isLittleEndian)
        {
            if (value == null)
                throw new ArgumentNullException("value");

            if ((uint)startIndex >= value.Length)
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_Index");

            if (startIndex > value.Length - 8)
                throw new ArgumentException("Arg_ArrayPlusOffTooSmall");

            fixed (byte* pbyte = &value[startIndex])
            {
                if (isLittleEndian)
                {
                    if (startIndex % 8 == 0) // data is aligned
                        return *((long*)pbyte);

                    int i1 = (*pbyte) | (*(pbyte + 1) << 8) | (*(pbyte + 2) << 16) | (*(pbyte + 3) << 24);
                    int i2 = (*(pbyte + 4)) | (*(pbyte + 5) << 8) | (*(pbyte + 6) << 16) | (*(pbyte + 7) << 24);
                    return (uint)i1 | ((long)i2 << 32);
                }
                else
                {
                    int i1 = (*pbyte << 24) | (*(pbyte + 1) << 16) | (*(pbyte + 2) << 8) | (*(pbyte + 3));
                    int i2 = (*(pbyte + 4) << 24) | (*(pbyte + 5) << 16) | (*(pbyte + 6) << 8) | (*(pbyte + 7));
                    return (uint)i2 | ((long)i1 << 32);
                }
            }
        }


        // Converts an array of bytes into an ushort.
        //
        [CLSCompliant(false)]
        public static ushort ToUInt16(byte[] value, int startIndex, bool isLittleEndian)
        {
            return (ushort)ToInt16(value, startIndex, isLittleEndian);
        }

        // Converts an array of bytes into an uint.
        //
        [CLSCompliant(false)]
        public static uint ToUInt32(byte[] value, int startIndex, bool isLittleEndian)
        {
            return (uint)ToInt32(value, startIndex, isLittleEndian);
        }

        // Converts an array of bytes into an unsigned long.
        //
        [CLSCompliant(false)]
        public static ulong ToUInt64(byte[] value, int startIndex, bool isLittleEndian)
        {
            return (ulong)ToInt64(value, startIndex, isLittleEndian);
        }

        // Converts an array of bytes into a float.
        unsafe public static float ToSingle(byte[] value, int startIndex, bool isLittleEndian)
        {
            int val = ToInt32(value, startIndex, isLittleEndian);
            return *(float*)&val;
        }

        // Converts an array of bytes into a double.
        unsafe public static double ToDouble(byte[] value, int startIndex, bool isLittleEndian)
        {
            long val = ToInt64(value, startIndex, isLittleEndian);
            return *(double*)&val;
        }

        private static char GetHexValue(int i)
        {
            System.Diagnostics.Debug.Assert(i >= 0 && i < 16, "i is out of range.");
            if (i < 10)
            {
                return (char)(i + '0');
            }

            return (char)(i - 10 + 'A');
        }

        // Converts an array of bytes into a String.
        public static String ToString(byte[] value, int startIndex, int length)
        {

            if (value == null)
            {
                throw new ArgumentNullException("byteArray");
            }

            int arrayLen = value.Length;
            if (startIndex < 0 || (startIndex >= arrayLen && startIndex > 0))
            {
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_StartIndex");
            }

            int realLength = length;
            if (realLength < 0)
            {
                throw new ArgumentOutOfRangeException("length", "ArgumentOutOfRange_GenericPositive");
            }

            if (startIndex > arrayLen - realLength)
            {
                throw new ArgumentException("Arg_ArrayPlusOffTooSmall");
            }

            if (realLength == 0)
            {
                return string.Empty;
            }

            char[] chArray = new char[realLength * 3];
            int i = 0;
            int index = startIndex;
            for (i = 0; i < realLength * 3; i += 3)
            {
                byte b = value[index++];
                chArray[i] = GetHexValue(b / 16);
                chArray[i + 1] = GetHexValue(b % 16);
                chArray[i + 2] = '-';
            }

            // We don't need the last '-' character
            return new String(chArray, 0, chArray.Length - 1);
        }

        // Converts an array of bytes into a String.
        public static String ToString(byte[] value)
        {
            if (value == null)
                throw new ArgumentNullException("value");
            return ToString(value, 0, value.Length);
        }

        // Converts an array of bytes into a String.
        public static String ToString(byte[] value, int startIndex)
        {
            if (value == null)
                throw new ArgumentNullException("value");
            return ToString(value, startIndex, value.Length - startIndex);
        }

        /*==================================ToBoolean===================================
        **Action:  Convert an array of bytes to a boolean value.  We treat this array
        **         as if the first 4 bytes were an Int4 an operate on this value.
        **Returns: True if the Int4 value of the first 4 bytes is non-zero.
        **Arguments: value -- The byte array
        **           startIndex -- The position within the array.
        **Exceptions: See ToInt4.
        ==============================================================================*/
        // Converts an array of bytes into a boolean.
        public static bool ToBoolean(byte[] value, int startIndex)
        {
            if (value == null)
                throw new ArgumentNullException("value");
            if (startIndex < 0)
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_NeedNonNegNum");
            if (startIndex > value.Length - 1)
                throw new ArgumentOutOfRangeException("startIndex", "ArgumentOutOfRange_Index");

            return value[startIndex] != 0;
        }

        public static unsafe long DoubleToInt64Bits(double value)
        {
            return *((long*)&value);
        }

        public static unsafe double Int64BitsToDouble(long value)
        {
            return *((double*)&value);
        }
    }


}