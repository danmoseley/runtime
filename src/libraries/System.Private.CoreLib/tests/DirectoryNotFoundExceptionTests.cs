using System;
using Xunit;
using System.IO;

namespace System.IO.Tests
{
    public class DirectoryNotFoundExceptionTests
    {
        [Fact]
        public void Ctor_DirectoryPath_SetsProperty()
        {
            var ex = new DirectoryNotFoundException("msg", "/tmp/foo");
            Assert.Equal("/tmp/foo", ex.DirectoryPath);
            Assert.Equal("msg", ex.Message);
            Assert.Equal(unchecked((int)0x80070003), ex.HResult);
        }

        [Fact]
        public void Ctor_DirectoryPath_NullPath()
        {
            var ex = new DirectoryNotFoundException("msg", (string?)null);
            Assert.Null(ex.DirectoryPath);
            Assert.Equal("msg", ex.Message);
        }

        [Fact]
        public void Ctor_NullMessage_DirectoryPath_UsesDefaultMessageWithPath()
        {
            var ex = new DirectoryNotFoundException(null, "/tmp/foo");
            Assert.Equal("/tmp/foo", ex.DirectoryPath);
            Assert.Contains("/tmp/foo", ex.Message);
        }

        [Fact]
        public void Ctor_DirectoryPath_InnerException()
        {
            var inner = new Exception("inner");
            var ex = new DirectoryNotFoundException("msg", "/tmp/foo", inner);
            Assert.Equal("/tmp/foo", ex.DirectoryPath);
            Assert.Equal("msg", ex.Message);
            Assert.Same(inner, ex.InnerException);
            Assert.Equal(unchecked((int)0x80070003), ex.HResult);
        }

        [Fact]
        public void Ctor_NoDirectoryPath_PropertyIsNull()
        {
            var ex = new DirectoryNotFoundException("msg");
            Assert.Null(ex.DirectoryPath);
        }
    }
}
