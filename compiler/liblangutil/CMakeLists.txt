# Solidity Commons Library (Solidity related sharing bits between libsolidity and libyul)
set(sources
	Common.h
	CharStream.cpp
	CharStream.h
	DebugData.h
	DebugInfoSelection.cpp
	DebugInfoSelection.h
	ErrorReporter.cpp
	ErrorReporter.h
	Exceptions.cpp
	Exceptions.h
	ParserBase.cpp
	ParserBase.h
	Scanner.cpp
	Scanner.h
	CharStreamProvider.h
	SemVerHandler.cpp
	SemVerHandler.h
	SourceLocation.h
	SourceLocation.cpp
	SourceReferenceExtractor.cpp
	SourceReferenceExtractor.h
	SourceReferenceFormatter.cpp
	SourceReferenceFormatter.h
	Token.cpp
	Token.h
	UniqueErrorReporter.h

	TVMVersion.h
)

add_library(langutil ${sources})
target_link_libraries(langutil PUBLIC solutil fmt::fmt-header-only)

install(TARGETS langutil
		EXPORT SolidityTargets
		ARCHIVE DESTINATION lib
		LIBRARY DESTINATION lib
		RUNTIME DESTINATION bin
)
