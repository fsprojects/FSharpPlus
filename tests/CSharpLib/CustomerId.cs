using System;

namespace CSharpLib
{
    public struct CustomerId : IEquatable<CustomerId>
    {
        public long Value { get; }
        public CustomerId(long value) => this.Value = value;
        public static readonly CustomerId Empty = new CustomerId();
        public bool Equals(CustomerId other) => !ReferenceEquals(null, other) && Equals(Value, other.Value);
        public override bool Equals(object obj) => obj is CustomerId id && Equals(id);
        public override int GetHashCode() => Value.GetHashCode();
        public override string ToString() => Value.ToString();
        public static bool TryParse(string value,out CustomerId id)
        {
            var res = value.Split('_');
            if (res.Length == 2 && res[0] == "C" && long.TryParse(res[1],out var val))
            {
                id=new CustomerId(val);
                return true;
            }
            id = Empty;
            return false;
        }
    }
}
