using System;
using Xunit;
using System.IO;

namespace System.IO.Tests
{
    public class DirectoryNotFoundExceptionTests
    {
        [Fact]
        public void DirectoryNotFoundException_DirectoryPath_Message()
        {
            var ex = new DirectoryNotFoundException(null, "/tmp/foo");
            Assert.Equal("/tmp/foo", ex.DirectoryPath);
            Assert.Contains("/tmp/foo", ex.Message);
        }
    }
}
