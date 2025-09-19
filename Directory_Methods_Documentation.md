# System.IO.Directory Class - Complete Method Documentation

## Overview

The `System.IO.Directory` class provides static methods for creating, moving, and enumerating through directories and subdirectories. This class cannot be inherited.

**Namespace:** System.IO  
**Assemblies:** netstandard.dll, System.Runtime.dll  
**Source:** [Directory.cs](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Private.CoreLib/src/System/IO/Directory.cs)

## Methods

### CreateDirectory Methods

#### CreateDirectory(String)
Creates all directories and subdirectories in the specified path unless they already exist.

```csharp
public static DirectoryInfo CreateDirectory(string path)
```

**Parameters:**
- `path` (String): The directory to create.

**Returns:** DirectoryInfo - An object that represents the directory at the specified path. This object is returned regardless of whether a directory at the specified path already exists.

**Exceptions:**
- `IOException`: The directory specified by path is a file, or the network name is not known.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: path is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `DirectoryNotFoundException`: The specified path is invalid (for example, it is on an unmapped drive).
- `NotSupportedException`: path contains a colon character (:) that is not part of a drive label.

#### CreateDirectory(String, UnixFileMode)
Creates all directories and subdirectories in the specified path with the specified permissions unless they already exist.

```csharp
[UnsupportedOSPlatform("windows")]
public static DirectoryInfo CreateDirectory(string path, UnixFileMode unixCreateMode)
```

**Parameters:**
- `path` (String): The directory to create.
- `unixCreateMode` (UnixFileMode): A bitwise combination of the enumeration values that specifies the Unix file mode used to create directories.

**Returns:** DirectoryInfo - An object that represents the directory at the specified path.

**Exceptions:**
- `ArgumentException`: path is a zero-length string, or contains one or more invalid characters, or the file mode is invalid.
- `ArgumentNullException`: path is null.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `PathTooLongException`: The specified path exceeds the system-defined maximum length.
- `IOException`: path is a file.
- `DirectoryNotFoundException`: A component of the path is not a directory.

### CreateSymbolicLink Method

#### CreateSymbolicLink(String, String)
Creates a directory symbolic link identified by path that points to pathToTarget.

```csharp
public static FileSystemInfo CreateSymbolicLink(string path, string pathToTarget)
```

**Parameters:**
- `path` (String): The absolute path where the symbolic link should be created.
- `pathToTarget` (String): The target directory of the symbolic link.

**Returns:** FileSystemInfo - A DirectoryInfo instance that wraps the newly created directory symbolic link.

**Exceptions:**
- `ArgumentNullException`: path or pathToTarget is null.
- `ArgumentException`: path or pathToTarget is empty, path is not an absolute path, or path or pathToTarget contains invalid path characters.
- `IOException`: A file or directory already exists in the location of path, or an I/O error occurred.

### CreateTempSubdirectory Method

#### CreateTempSubdirectory(String)
Creates a uniquely-named, empty directory in the current user's temporary directory.

```csharp
public static DirectoryInfo CreateTempSubdirectory(string? prefix = null)
```

**Parameters:**
- `prefix` (String, optional): An optional string to add to the beginning of the subdirectory name.

**Returns:** DirectoryInfo - An object that represents the directory that was created.

**Exceptions:**
- `ArgumentException`: prefix contains a directory separator.
- `IOException`: A new directory cannot be created.

### Delete Methods

#### Delete(String)
Deletes an empty directory from a specified path.

```csharp
public static void Delete(string path)
```

**Parameters:**
- `path` (String): The name of the empty directory to remove. This directory must be writable and empty.

**Exceptions:**
- `IOException`: A file with the same name and location specified by path exists, the directory is the application's current working directory, the directory specified by path is not empty, the directory is read-only or contains a read-only file, or the directory is being used by another process.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: path is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `DirectoryNotFoundException`: path does not exist or could not be found, or the specified path is invalid.

#### Delete(String, Boolean)
Deletes the specified directory and, if indicated, any subdirectories and files in the directory.

```csharp
public static void Delete(string path, bool recursive)
```

**Parameters:**
- `path` (String): The name of the directory to remove.
- `recursive` (Boolean): true to remove directories, subdirectories, and files in path; otherwise, false.

**Exceptions:**
- `IOException`: A file with the same name and location specified by path exists, the directory specified by path is read-only or recursive is false and path is not an empty directory, the directory is the application's current working directory, the directory contains a read-only file, or the directory is being used by another process.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: path is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `DirectoryNotFoundException`: path does not exist or could not be found, or the specified path is invalid.

### EnumerateDirectories Methods

#### EnumerateDirectories(String)
Returns an enumerable collection of directory full names in a specified path.

```csharp
public static IEnumerable<string> EnumerateDirectories(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the directories in the directory specified by path.

#### EnumerateDirectories(String, String)
Returns an enumerable collection of directory full names that match a search pattern in a specified path.

```csharp
public static IEnumerable<string> EnumerateDirectories(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.
- `searchPattern` (String): The search string to match against the names of subdirectories in path. This parameter can contain a combination of valid literal and wildcard characters, but it doesn't support regular expressions.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the directories in the directory specified by path and that match the specified search pattern.

#### EnumerateDirectories(String, String, SearchOption)
Returns an enumerable collection of directory full names that match a search pattern in a specified path, and optionally searches subdirectories.

```csharp
public static IEnumerable<string> EnumerateDirectories(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of subdirectories in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include all subdirectories or only the current directory.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the directories in the directory specified by path and that match the specified search pattern and search option.

#### EnumerateDirectories(String, String, EnumerationOptions)
Returns an enumerable collection of the directory full names that match a search pattern in a specified path, and optionally searches subdirectories.

```csharp
public static IEnumerable<string> EnumerateDirectories(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of subdirectories in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the directories in the directory specified by path and that match the specified search pattern and enumeration options.

### EnumerateFiles Methods

#### EnumerateFiles(String)
Returns an enumerable collection of full file names in a specified path.

```csharp
public static IEnumerable<string> EnumerateFiles(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the files in the directory specified by path.

**Exceptions:**
- `ArgumentException`: path is a zero-length string, contains only white space, or contains invalid characters.
- `ArgumentNullException`: path is null.
- `DirectoryNotFoundException`: path is invalid, such as referring to an unmapped drive.
- `IOException`: path is a file name.
- `PathTooLongException`: The specified path, file name, or combined exceed the system-defined maximum length.
- `SecurityException`: The caller does not have the required permission.
- `UnauthorizedAccessException`: The caller does not have the required permission.

#### EnumerateFiles(String, String)
Returns an enumerable collection of full file names that match a search pattern in a specified path.

```csharp
public static IEnumerable<string> EnumerateFiles(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.
- `searchPattern` (String): The search string to match against the names of files in path. This parameter can contain a combination of valid literal path and wildcard (* and ?) characters, but it doesn't support regular expressions.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the files in the directory specified by path and that match the specified search pattern.

#### EnumerateFiles(String, String, SearchOption)
Returns an enumerable collection of full file names that match a search pattern in a specified path, and optionally searches subdirectories.

```csharp
public static IEnumerable<string> EnumerateFiles(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of files in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include all subdirectories or only the current directory.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the files in the directory specified by path and that match the specified search pattern and search option.

#### EnumerateFiles(String, String, EnumerationOptions)
Returns an enumerable collection of full file names that match a search pattern and enumeration options in a specified path, and optionally searches subdirectories.

```csharp
public static IEnumerable<string> EnumerateFiles(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of files in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of the full names (including paths) for the files in the directory specified by path and that match the specified search pattern and enumeration options.

### EnumerateFileSystemEntries Methods

#### EnumerateFileSystemEntries(String)
Returns an enumerable collection of file names and directory names in a specified path.

```csharp
public static IEnumerable<string> EnumerateFileSystemEntries(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of file-system entries in the directory specified by path.

#### EnumerateFileSystemEntries(String, String)
Returns an enumerable collection of file names and directory names that match a search pattern in a specified path.

```csharp
public static IEnumerable<string> EnumerateFileSystemEntries(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against file-system entries in path.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of file-system entries in the directory specified by path and that match the specified search pattern.

#### EnumerateFileSystemEntries(String, String, SearchOption)
Returns an enumerable collection of file names and directory names that match a search pattern in a specified path, and optionally searches subdirectories.

```csharp
public static IEnumerable<string> EnumerateFileSystemEntries(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against file-system entries in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include only the current directory or should include all subdirectories.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of file-system entries in the directory specified by path and that match the specified search pattern and search option.

#### EnumerateFileSystemEntries(String, String, EnumerationOptions)
Returns an enumerable collection of file names and directory names that match a search pattern and enumeration options in a specified path.

```csharp
public static IEnumerable<string> EnumerateFileSystemEntries(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against file-system entries in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** IEnumerable&lt;String&gt; - An enumerable collection of file-system entries in the directory specified by path and that match the specified search pattern and enumeration options.

### Exists Method

#### Exists(String)
Determines whether the given path refers to an existing directory on disk.

```csharp
public static bool Exists([NotNullWhen(true)] string? path)
```

**Parameters:**
- `path` (String): The path to test.

**Returns:** Boolean - true if path refers to an existing directory; false if the directory does not exist or an error occurs when trying to determine if the specified directory exists.

### GetDirectories Methods

#### GetDirectories(String)
Returns the names of subdirectories (including their paths) in the specified directory.

```csharp
public static string[] GetDirectories(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.

**Returns:** String[] - An array of the full names (including paths) of subdirectories in the specified path, or an empty array if no directories are found.

**Exceptions:**
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: path is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `IOException`: path is a file name, or file or directory is corrupted and unreadable.
- `DirectoryNotFoundException`: The specified path is invalid (for example, it is on an unmapped drive).

#### GetDirectories(String, String)
Returns the names of subdirectories (including their paths) that match the specified search pattern in the specified directory.

```csharp
public static string[] GetDirectories(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of subdirectories in path.

**Returns:** String[] - An array of the full names (including paths) of the subdirectories that match the search pattern in the specified directory, or an empty array if no directories are found.

#### GetDirectories(String, String, SearchOption)
Returns the names of the subdirectories (including their paths) that match the specified search pattern in the specified directory, and optionally searches subdirectories.

```csharp
public static string[] GetDirectories(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of subdirectories in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include all subdirectories or only the current directory.

**Returns:** String[] - An array of the full names (including paths) of the subdirectories that match the specified criteria, or an empty array if no directories are found.

#### GetDirectories(String, String, EnumerationOptions)
Returns the names of subdirectories (including their paths) that match the specified search pattern and enumeration options in the specified directory.

```csharp
public static string[] GetDirectories(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of subdirectories in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** String[] - An array of the full names (including paths) of the subdirectories that match the specified search pattern and enumeration options in the specified directory, or an empty array if no directories are found.

### GetFiles Methods

#### GetFiles(String)
Returns the names of files (including their paths) in the specified directory.

```csharp
public static string[] GetFiles(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search. This string is not case-sensitive.

**Returns:** String[] - An array of the full names (including paths) for the files in the specified directory, or an empty array if no files are found.

**Exceptions:**
- `IOException`: path is a file name, or a network error has occurred.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: path is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `DirectoryNotFoundException`: The specified path is not found or is invalid.

#### GetFiles(String, String)
Returns the names of files (including their paths) that match the specified search pattern in the specified directory.

```csharp
public static string[] GetFiles(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of files in path.

**Returns:** String[] - An array of the full names (including paths) for the files in the specified directory that match the specified search pattern, or an empty array if no files are found.

#### GetFiles(String, String, SearchOption)
Returns the names of files (including their paths) that match the specified search pattern in the specified directory, using a value to determine whether to search subdirectories.

```csharp
public static string[] GetFiles(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of files in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include all subdirectories or only the current directory.

**Returns:** String[] - An array of the full names (including paths) for the files in the specified directory that match the specified search pattern and option, or an empty array if no files are found.

#### GetFiles(String, String, EnumerationOptions)
Returns the names of files (including their paths) that match the specified search pattern and enumeration options in the specified directory.

```csharp
public static string[] GetFiles(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of files in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** String[] - An array of the full names (including paths) for the files in the specified directory that match the specified search pattern and enumeration options, or an empty array if no files are found.

### GetFileSystemEntries Methods

#### GetFileSystemEntries(String)
Returns the names of all files and subdirectories in a specified path.

```csharp
public static string[] GetFileSystemEntries(string path)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.

**Returns:** String[] - An array of the names of files and subdirectories in the specified directory, or an empty array if no files or subdirectories are found.

#### GetFileSystemEntries(String, String)
Returns an array of file names and directory names that match a search pattern in a specified path.

```csharp
public static string[] GetFileSystemEntries(string path, string searchPattern)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of file-system entries in path.

**Returns:** String[] - An array of file names and directory names that match the specified search criteria, or an empty array if no files or directories are found.

#### GetFileSystemEntries(String, String, SearchOption)
Returns an array of all the file names and directory names that match a search pattern in a specified path, and optionally searches subdirectories.

```csharp
public static string[] GetFileSystemEntries(string path, string searchPattern, SearchOption searchOption)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of file-system entries in path.
- `searchOption` (SearchOption): One of the enumeration values that specifies whether the search operation should include only the current directory or should include all subdirectories.

**Returns:** String[] - An array of file names and directory names that match the specified search criteria, or an empty array if no files or directories are found.

#### GetFileSystemEntries(String, String, EnumerationOptions)
Returns an array of file names and directory names that match a search pattern and enumeration options in a specified path.

```csharp
public static string[] GetFileSystemEntries(string path, string searchPattern, EnumerationOptions enumerationOptions)
```

**Parameters:**
- `path` (String): The relative or absolute path to the directory to search.
- `searchPattern` (String): The search string to match against the names of file-system entries in path.
- `enumerationOptions` (EnumerationOptions): An object that describes the search and enumeration configuration to use.

**Returns:** String[] - An array of file names and directory names that match the specified search pattern and enumeration options, or an empty array if no files or directories are found.

### Directory Information Methods

#### GetCurrentDirectory()
Gets the current working directory of the application.

```csharp
public static string GetCurrentDirectory()
```

**Returns:** String - A string that contains the absolute path of the current working directory, and does not end with a backslash (\).

#### SetCurrentDirectory(String)
Sets the application's current working directory to the specified directory.

```csharp
public static void SetCurrentDirectory(string path)
```

**Parameters:**
- `path` (String): The path to which the current working directory is set.

**Exceptions:**
- `ArgumentException`: path is a zero-length string, contains only white space, or contains one or more invalid characters.

#### GetDirectoryRoot(String)
Returns the volume information, root information, or both for the specified path.

```csharp
public static string GetDirectoryRoot(string path)
```

**Parameters:**
- `path` (String): The path of a file or directory.

**Returns:** String - A string that contains the volume information, root information, or both for the specified path.

**Exceptions:**
- `ArgumentNullException`: path is null.

#### GetParent(String)
Retrieves the parent directory of the specified path, including both absolute and relative paths.

```csharp
public static DirectoryInfo? GetParent(string path)
```

**Parameters:**
- `path` (String): The path for which to retrieve the parent directory.

**Returns:** DirectoryInfo - The parent directory, or null if path is the root directory, including the root of a UNC server or share name.

### Time-Related Methods

#### GetCreationTime(String)
Gets the creation date and time of a directory.

```csharp
public static DateTime GetCreationTime(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the creation date and time for the specified directory. This value is expressed in local time.

#### GetCreationTimeUtc(String)
Gets the creation date and time, in Coordinated Universal Time (UTC) format, of a directory.

```csharp
public static DateTime GetCreationTimeUtc(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the creation date and time for the specified directory. This value is expressed in UTC time.

#### SetCreationTime(String, DateTime)
Sets the creation date and time for the specified file or directory.

```csharp
public static void SetCreationTime(string path, DateTime creationTime)
```

**Parameters:**
- `path` (String): The path of the directory.
- `creationTime` (DateTime): The date and time the directory was created. This value is expressed in local time.

#### SetCreationTimeUtc(String, DateTime)
Sets the creation date and time, in Coordinated Universal Time (UTC) format, for the specified file or directory.

```csharp
public static void SetCreationTimeUtc(string path, DateTime creationTimeUtc)
```

**Parameters:**
- `path` (String): The path of the directory.
- `creationTimeUtc` (DateTime): The date and time the directory was created. This value is expressed in UTC time.

#### GetLastAccessTime(String)
Returns the date and time the specified file or directory was last accessed.

```csharp
public static DateTime GetLastAccessTime(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the date and time the specified directory was last accessed. This value is expressed in local time.

#### GetLastAccessTimeUtc(String)
Returns the date and time, in Coordinated Universal Time (UTC) format, that the specified file or directory was last accessed.

```csharp
public static DateTime GetLastAccessTimeUtc(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the date and time the specified directory was last accessed. This value is expressed in UTC time.

#### SetLastAccessTime(String, DateTime)
Sets the date and time the specified file or directory was last accessed.

```csharp
public static void SetLastAccessTime(string path, DateTime lastAccessTime)
```

**Parameters:**
- `path` (String): The path of the directory.
- `lastAccessTime` (DateTime): The date and time the directory was last accessed. This value is expressed in local time.

#### SetLastAccessTimeUtc(String, DateTime)
Sets the date and time, in Coordinated Universal Time (UTC) format, that the specified file or directory was last accessed.

```csharp
public static void SetLastAccessTimeUtc(string path, DateTime lastAccessTimeUtc)
```

**Parameters:**
- `path` (String): The path of the directory.
- `lastAccessTimeUtc` (DateTime): The date and time the directory was last accessed. This value is expressed in UTC time.

#### GetLastWriteTime(String)
Returns the date and time the specified file or directory was last written to.

```csharp
public static DateTime GetLastWriteTime(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the date and time the specified directory was last written to. This value is expressed in local time.

#### GetLastWriteTimeUtc(String)
Returns the date and time, in Coordinated Universal Time (UTC) format, that the specified file or directory was last written to.

```csharp
public static DateTime GetLastWriteTimeUtc(string path)
```

**Parameters:**
- `path` (String): The path of the directory.

**Returns:** DateTime - A structure that is set to the date and time the specified directory was last written to. This value is expressed in UTC time.

#### SetLastWriteTime(String, DateTime)
Sets the date and time a directory was last written to.

```csharp
public static void SetLastWriteTime(string path, DateTime lastWriteTime)
```

**Parameters:**
- `path` (String): The path of the directory.
- `lastWriteTime` (DateTime): The date and time the directory was last written to. This value is expressed in local time.

#### SetLastWriteTimeUtc(String, DateTime)
Sets the date and time, in Coordinated Universal Time (UTC) format, that a directory was last written to.

```csharp
public static void SetLastWriteTimeUtc(string path, DateTime lastWriteTimeUtc)
```

**Parameters:**
- `path` (String): The path of the directory.
- `lastWriteTimeUtc` (DateTime): The date and time the directory was last written to. This value is expressed in UTC time.

### Move Method

#### Move(String, String)
Moves a file or a directory and its contents to a new location.

```csharp
public static void Move(string sourceDirName, string destDirName)
```

**Parameters:**
- `sourceDirName` (String): The path of the file or directory to move.
- `destDirName` (String): The path to the new location for sourceDirName or its contents. If sourceDirName is a file, then destDirName must also be a file name.

**Exceptions:**
- `IOException`: An attempt was made to move a directory to a different volume, destDirName already exists, the sourceDirName and destDirName parameters refer to the same file or directory, or the directory or a file within it is being used by another process.
- `UnauthorizedAccessException`: The caller does not have the required permission.
- `ArgumentException`: sourceDirName or destDirName is a zero-length string, contains only white space, or contains one or more invalid characters.
- `ArgumentNullException`: sourceDirName or destDirName is null.
- `PathTooLongException`: The specified path, file name, or both exceed the system-defined maximum length.
- `DirectoryNotFoundException`: The path specified by sourceDirName is invalid.

### System Information Methods

#### GetLogicalDrives()
Retrieves the names of the logical drives on this computer.

```csharp
public static string[] GetLogicalDrives()
```

**Returns:** String[] - An array that contains the logical drives on this computer.

### Link Methods

#### ResolveLinkTarget(String, Boolean)
Gets the target of the specified directory link.

```csharp
public static FileSystemInfo? ResolveLinkTarget(string linkPath, bool returnFinalTarget)
```

**Parameters:**
- `linkPath` (String): The path of the directory link.
- `returnFinalTarget` (Boolean): true to follow links to the final target; false to return the immediate next link.

**Returns:** FileSystemInfo - A DirectoryInfo instance if linkPath exists, independently if the target exists or not. null if linkPath is not a link.

**Exceptions:**
- `IOException`: The directory on linkPath does not exist, the link's file system entry type is inconsistent with that of its target, or too many levels of symbolic links.

## Remarks

- Use the Directory class for typical operations such as copying, moving, renaming, creating, and deleting directories.
- The static methods of the Directory class perform security checks on all methods. If you are going to reuse an object several times, consider using the corresponding instance method of DirectoryInfo instead.
- Most Directory methods require the path to the directory that you are manipulating.
- In members that accept a path, the path can refer to a file or a directory. You can use a full path, a relative path, or a Universal Naming Convention (UNC) path.
- In members that accept a searchPattern parameter, the search string can be any combination of literal characters and two wildcard characters: * and ?. This parameter does not recognize regular expressions.
- The EnumerateFiles and GetFiles methods differ as follows: When you use EnumerateFiles, you can start enumerating the collection of names before the whole collection is returned; when you use GetFiles, you must wait for the whole array of names to be returned before you can access the array.

## Common Wildcard Patterns

| Wildcard specifier | Matches |
| --- | --- |
| * (asterisk) | Zero or more characters in that position. |
| ? (question mark) | Exactly one character in that position. |

## See Also

- [DirectoryInfo Class](https://learn.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- [File Class](https://learn.microsoft.com/en-us/dotnet/api/system.io.file)
- [Path Class](https://learn.microsoft.com/en-us/dotnet/api/system.io.path)
- [Common I/O Tasks](https://learn.microsoft.com/en-us/dotnet/standard/io/common-i-o-tasks)
- [File and Stream I/O](https://learn.microsoft.com/en-us/dotnet/standard/io/)