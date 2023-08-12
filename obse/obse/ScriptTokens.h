#pragma once

#if _DEBUG
#define DBG_EXPR_LEAKS 1
struct Operand;
extern SInt32 TOKEN_COUNT;
extern SInt32 EXPECTED_TOKEN_COUNT;
extern SInt32 FUNCTION_CONTEXT_COUNT;
#endif

#include "Script.h"
#include "CommandTable.h"
#include "GameForms.h"
#include "ArrayVar.h"

#if OBLIVION
#include "StringVar.h"
#include "GameAPI.h"

#endif

struct Operator;
struct SliceToken;
struct ArrayElementToken;
struct ForEachContext;
class ExpressionEvaluator;
struct ScriptToken;

enum OperatorType : UInt8
{
	kOpType_Min		= 0,

	kOpType_Assignment	= 0,
	kOpType_LogicalOr,
	kOpType_LogicalAnd,
	kOpType_Slice,
	kOpType_Equals,
	kOpType_NotEqual,
	kOpType_GreaterThan,
	kOpType_LessThan,
	kOpType_GreaterOrEqual,
	kOpType_LessOrEqual,
	kOpType_BitwiseOr,
	kOpType_BitwiseAnd,
	kOpType_LeftShift,
	kOpType_RightShift,
	kOpType_Add,
	kOpType_Subtract,
	kOpType_Multiply,
	kOpType_Divide,
	kOpType_Modulo,
	kOpType_Exponent,
	kOpType_Negation,
	kOpType_LogicalNot,
	kOpType_LeftParen,
	kOpType_RightParen,
	kOpType_LeftBracket,
	kOpType_RightBracket,
	kOpType_In,				// '<-'
	kOpType_ToString,		// '$'
	kOpType_PlusEquals,
	kOpType_TimesEquals,
	kOpType_DividedEquals,
	kOpType_ExponentEquals,
	kOpType_MinusEquals,
	kOpType_ToNumber,		// '#'
	kOpType_Dereference,	// unary '*'
	kOpType_MemberAccess,	// stringmap->string, shortcut for stringmap["string"]
	kOpType_MakePair,		// 'a::b', e.g. for defining key-value pairs for map structures
	kOpType_Box,			// unary; wraps a value in a single-element array

	kOpType_Max
};

enum Token_Type : UInt8
{
	kTokenType_Number	= 0,
	kTokenType_Boolean,
	kTokenType_String,
	kTokenType_Form,
	kTokenType_Ref,
	kTokenType_Global,
	kTokenType_Array,
	kTokenType_ArrayElement,
	kTokenType_Slice,
	kTokenType_Command,
	kTokenType_Variable,
	kTokenType_NumericVar,
	kTokenType_RefVar,
	kTokenType_StringVar,
	kTokenType_ArrayVar,
	kTokenType_Ambiguous,
	kTokenType_Operator,
	kTokenType_ForEachContext,

	// numeric literals can optionally be encoded as one of the following
	// all are converted to _Number on evaluation
	kTokenType_Byte,
	kTokenType_Short,		// 2 bytes
	kTokenType_Int,			// 4 bytes

	kTokenType_Pair,
	kTokenType_AssignableString,

	kTokenType_Invalid,
	kTokenType_Max = kTokenType_Invalid,

	// sigil value, returned when an empty expression is parsed
	kTokenType_Empty = kTokenType_Max + 1,
};

constexpr auto GetTokenConvertibilityMap()
{
	// Maps a token type to multiple other token types it is convertible to.
	// A simple array of pairs, the first element denoting the 'from' and the second the 'to'.

	constexpr std::array TokenConvertibilityMap
	{
		std::pair{ Token_Type::kTokenType_Number, Token_Type::kTokenType_Boolean },

		std::pair{ Token_Type::kTokenType_Boolean, Token_Type::kTokenType_Number },

		std::pair{ Token_Type::kTokenType_Command, Token_Type::kTokenType_Number },
		std::pair{ Token_Type::kTokenType_Command, Token_Type::kTokenType_Form },
		std::pair{ Token_Type::kTokenType_Command, Token_Type::kTokenType_Boolean },
#if !OBLIVION
		std::pair{ Token_Type::kTokenType_Command, Token_Type::kTokenType_Ambiguous },
#endif

		std::pair{ Token_Type::kTokenType_Ref, Token_Type::kTokenType_Form },
		std::pair{ Token_Type::kTokenType_Ref, Token_Type::kTokenType_Boolean },

		std::pair{ Token_Type::kTokenType_Global, Token_Type::kTokenType_Number },
		std::pair{ Token_Type::kTokenType_Global, Token_Type::kTokenType_Boolean },

		std::pair{ Token_Type::kTokenType_Form, Token_Type::kTokenType_Boolean },

		std::pair{ Token_Type::kTokenType_NumericVar, Token_Type::kTokenType_Number },
		std::pair{ Token_Type::kTokenType_NumericVar, Token_Type::kTokenType_Boolean },
		std::pair{ Token_Type::kTokenType_NumericVar, Token_Type::kTokenType_Variable },

		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_Number },
		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_Form },
		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_String },
		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_Array },
		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_Boolean },
#if !OBLIVION
		std::pair{ Token_Type::kTokenType_ArrayElement, Token_Type::kTokenType_Ambiguous },
#endif

		std::pair{ Token_Type::kTokenType_RefVar, Token_Type::kTokenType_Form },
		std::pair{ Token_Type::kTokenType_RefVar, Token_Type::kTokenType_Boolean },
		std::pair{ Token_Type::kTokenType_RefVar, Token_Type::kTokenType_Variable },

		std::pair{ Token_Type::kTokenType_StringVar, Token_Type::kTokenType_String },
		std::pair{ Token_Type::kTokenType_StringVar, Token_Type::kTokenType_Boolean },
		std::pair{ Token_Type::kTokenType_StringVar, Token_Type::kTokenType_Variable },

		std::pair{ Token_Type::kTokenType_ArrayVar, Token_Type::kTokenType_Array },
		std::pair{ Token_Type::kTokenType_ArrayVar, Token_Type::kTokenType_Boolean },
		std::pair{ Token_Type::kTokenType_ArrayVar, Token_Type::kTokenType_Variable },

		std::pair{ Token_Type::kTokenType_Array, Token_Type::kTokenType_Boolean },

		std::pair{ Token_Type::kTokenType_AssignableString, Token_Type::kTokenType_String }
	};

	return TokenConvertibilityMap;
}

struct Slice		// a range used for indexing into a string or array, expressed as arr[a:b]
{
	bool			bIsString{false};
	double			m_lower{};
	double			m_upper{};
	std::string		m_lowerStr;
	std::string		m_upperStr;

	Slice(const Slice* _slice);
	Slice(const std::string& l, const std::string& u) : bIsString(true), m_lowerStr(l), m_upperStr(u) { }
	Slice(const double l, const double u) : m_lower(l), m_upper(u) { }
	Slice(const char* l, const char* u) : bIsString(true), m_lowerStr(l), m_upperStr(u) { }
	void GetArrayBounds(ArrayKey& lo, ArrayKey& hi) const;
};

struct TokenPair	// a pair of tokens, specified as 'a::b'
{
	ScriptToken	* left;
	ScriptToken * right;

	TokenPair(ScriptToken* l, ScriptToken* r);
	~TokenPair();
};

#if OBLIVION

struct ForEachContext
{
	UInt32				sourceID;
	UInt32				iteratorID;
	UInt32				variableType;
	ScriptEventList::Var * var;

	ForEachContext(UInt32 src, UInt32 iter, UInt32 varType, ScriptEventList::Var* _var) : sourceID(src), iteratorID(iter), variableType(varType), var(_var) { }
};

#endif

// Script token's value struct type, handling aliasing conversions on a piece of memory.
struct ScriptTokenValuePack
{
	std::string str;
	union
	{
		Script::RefVariable* refVar;
		UInt32 formID;
		double num;
		TESGlobal* global;
		Operator* op;
		// compile-time only
		Script::VariableInfo* varInfo;
		CommandInfo* cmd;
		ScriptToken* token;
#if OBLIVION
		// run-time only
		ArrayID arrID;
		ScriptEventList::Var* var;
#endif
	};
};

// ScriptToken Data
struct ScriptTokenData
{
	Token_Type Type{Token_Type::kTokenType_Invalid};
	UInt8 VariableType{};
	UInt16 RefIdx{};
	ScriptTokenValuePack Value;
};

struct SliceTokenData
{
	Token_Type Type{ Token_Type::kTokenType_Invalid };
	UInt8 VariableType{};
	UInt16 RefIdx{};
	ScriptTokenValuePack Value;

	bool bIsString{ false };
	double m_lower{};
	double m_upper{};
	std::string m_lowerStr;
	std::string m_upperStr;

	Slice slice;

	//SliceTokenData()
	//{
	//	//Slice(const Slice * _slice);
	//	//Slice(const std::string & l, const std::string & u) : bIsString(true), m_lowerStr(l), m_upperStr(u) { }
	//	//Slice(const double l, const double u) : m_lower(l), m_upper(u) { }
	//	//Slice(const char* l, const char* u) : bIsString(true), m_lowerStr(l), m_upperStr(u) { }

	//	switch(Type)
	//	{
	//		case kTokenType_Number:
	//			break;
	//		case kTokenType_Boolean:
	//			break;
	//		case kTokenType_String:
	//			bIsString = true;
	//			break;
	//		case kTokenType_Form:
	//			break;
	//		case kTokenType_Ref:
	//			break;
	//		case kTokenType_Global:
	//			break;
	//		case kTokenType_Array:
	//			break;
	//		case kTokenType_ArrayElement:
	//			break;
	//		case kTokenType_Slice:
	//			break;
	//		case kTokenType_Command:
	//			break;
	//		case kTokenType_Variable:
	//			break;
	//		case kTokenType_NumericVar:
	//			break;
	//		case kTokenType_RefVar:
	//			break;
	//		case kTokenType_StringVar:
	//			break;
	//		case kTokenType_ArrayVar:
	//			break;
	//		case kTokenType_Ambiguous:
	//			break;
	//		case kTokenType_Operator:
	//			break;
	//		case kTokenType_ForEachContext:
	//			break;
	//		case kTokenType_Byte:
	//			break;
	//		case kTokenType_Short:
	//			break;
	//		case kTokenType_Int:
	//			break;
	//		case kTokenType_Pair:
	//			break;
	//		case kTokenType_AssignableString:
	//			break;
	//		case kTokenType_Invalid:
	//			break;
	//		case kTokenType_Empty:
	//			break;
	//		default: ;
	//	}
	//}
};

struct ArrayElementTokenData
{
	Token_Type Type{ Token_Type::kTokenType_Invalid };
	UInt8 VariableType{};
	UInt16 RefIdx{};
	ScriptTokenValuePack Value;

	// TODO
	//ArrayKey Key;
};

// Functions to operate on the ScriptTokenData data
[[nodiscard]]
constexpr bool CanConvertOperand(Token_Type from, Token_Type to)
{
	// Change these to std::find_if and std::cend if don't have ranges.
	using std::ranges::find_if;
	using std::ranges::cend;

	if (from == to)
		return true;
	if (from >= kTokenType_Invalid || to >= kTokenType_Invalid)
		return false;

	static constexpr auto tokenMap = GetTokenConvertibilityMap();
	const auto findResult = find_if(tokenMap, [from, to](auto& kvp)
		{
			return (kvp.first == from && kvp.second == to);
		});

	if(findResult != cend(tokenMap))
	{
		return true;
	}

	return false;
}

[[nodiscard]]
inline auto GetString(const ScriptTokenData& data) -> const char*
{
	static const char* empty = "";
	const char* result = nullptr;

	if (data.Type == kTokenType_String)
		result = data.Value.str.c_str();
#if OBLIVION
	else if (data.Type == kTokenType_StringVar && data.Value.var)
	{
		StringVar* strVar = g_StringMap.Get(data.Value.var->data);
		result = strVar ? strVar->GetCString() : nullptr;
	}
#endif
	return result ? result : empty;
}

[[nodiscard]]
inline bool GetBool(const ScriptTokenData& data)
{
	switch (data.Type)
	{
	case kTokenType_Boolean:
	case kTokenType_Number:
		return data.Value.num ? true : false;
	case kTokenType_Form:
		return data.Value.formID ? true : false;
	case kTokenType_Global:
		return data.Value.global->data ? true : false;
#if OBLIVION
	case kTokenType_Array:
		return data.Value.arrID ? true : false;
	case kTokenType_NumericVar:
	case kTokenType_StringVar:
	case kTokenType_ArrayVar:
	case kTokenType_RefVar:
		return data.Value.var->data ? true : false;
#endif
	default:
		return false;
	}
}

[[nodiscard]]
inline auto GetFormId(const ScriptTokenData& data) -> UInt32
{
	if (data.Type == kTokenType_Form)
		return data.Value.formID;
#if OBLIVION
	if (data.Type == kTokenType_RefVar && data.Value.var)
		return *(UInt64*)(&data.Value.var->data);
#endif
	if (data.Type == kTokenType_Ref && data.Value.refVar)
		return data.Value.refVar->form ? data.Value.refVar->form->refID : 0;

	return 0;
}

[[nodiscard]]
inline auto GetTESForm(const ScriptTokenData& data) -> TESForm*
{
	// ###TODO: handle Ref (RefVariable)? Read() turns RefVariable into Form so that type is compile-time only
#if OBLIVION
	if (data.Type == kTokenType_Form)
		return LookupFormByID(data.Value.formID);
	if (data.Type == kTokenType_RefVar && data.Value.var)
		return LookupFormByID(*((UInt64*)(&data.Value.var->data)));
#endif

	if (data.Type == kTokenType_Ref && data.Value.refVar)
		return data.Value.refVar->form;

	return nullptr;
}

[[nodiscard]]
inline auto GetNumber(const ScriptTokenData& data) -> double
{
	if (data.Type == kTokenType_Number || data.Type == kTokenType_Boolean)
		return data.Value.num;
	if (data.Type == kTokenType_Global && data.Value.global)
		return data.Value.global->data;
#if OBLIVION
	if ((data.Type == kTokenType_NumericVar && data.Value.var) || (data.Type == kTokenType_StringVar && data.Value.var))
		return data.Value.var->data;
#endif

	return 0.0;
}

[[nodiscard]]
inline auto CanConvertTo(const ScriptTokenData& data, Token_Type toType)
{
	return CanConvertOperand(data.Type, toType);
}

// Functions to operate on the SliceTokenData data
[[nodiscard]]
inline auto GetSlice(const SliceTokenData& data) -> const Slice*
{
	return data.Type == kTokenType_Slice ? &data.slice : nullptr;
}

// Polymorphic behavior concept type, specifies a polymorphic interface of interest.
struct ScriptTokenBehaviorConcept
{
	virtual ~ScriptTokenBehaviorConcept() = 0;

	virtual const char* GetString() const = 0;
	virtual bool GetBool() const = 0;
	virtual UInt32 GetFormID() const = 0;
	virtual TESForm* GetTESForm() const = 0;
	virtual double GetNumber() const = 0;
	virtual const ArrayKey* GetArrayKey() const = 0;
	virtual const ForEachContext* GetForEachContext() const = 0;
	virtual const Slice* GetSlice() const = 0;

	virtual bool CanConvertTo(Token_Type to) const = 0; // behavior varies b/w compile/run-time for ambiguous types

	virtual ArrayID GetOwningArrayID() const = 0;
	virtual const ScriptToken* GetToken() const = 0;
	virtual const TokenPair* GetPair() const = 0;
	// if block for oblivion not necessary here.
	virtual ArrayID GetArray() const = 0;

};

// A concrete type to model the concept behavior, even for other data pack types that aren't part of the inheritance model.
template<class DataPack_t>
struct ScriptTokenBehaviorModel : ScriptTokenBehaviorConcept
{
	DataPack_t Data;

	// Function pointer wrappers, pointing to the actual implementation in use.
	std::function<const char*(DataPack_t&)> StringGetter;
	std::function<bool(DataPack_t&)> BoolGetter;
	std::function<UInt32(DataPack_t&)> FormIDGetter;
	std::function<TESForm*(DataPack_t&)> TESFormGetter;
	std::function<double(DataPack_t&)> NumberGetter;
	std::function<const ArrayKey*(DataPack_t&)> ArrayKeyGetter;
	std::function<const ForEachContext* (DataPack_t&)> ForEachContextGetter;
	std::function<const Slice*(DataPack_t&)> SliceGetter;
	std::function<bool(DataPack_t&, Token_Type)> CanConvertToGetter;
	std::function<ArrayID(DataPack_t&)> OwningArrayIDGetter;
	//std::function<const ScriptTokenData*(DataPack_t&)> ScriptTokenDataGetter;
	std::function<const TokenPair*(DataPack_t&)> TokenPairGetter;
	std::function<ArrayID(DataPack_t&)> ArrayIDGetter;


	~ScriptTokenBehaviorModel() override = default;

	// Polymorphic interface implementations, forwarding to the runtime polymorphism capable function pointer wrappers.
	[[nodiscard]] const char* GetString() const override { return StringGetter ? StringGetter(Data) : ""; }
	[[nodiscard]] bool GetBool() const override { return BoolGetter ? BoolGetter(Data) : false; }
	[[nodiscard]] UInt32 GetFormID() const override { return FormIDGetter ? FormIDGetter(Data) : 0; }
	[[nodiscard]] TESForm* GetTESForm() const override { return TESFormGetter ? TESFormGetter(Data) : nullptr; }
	[[nodiscard]] double GetNumber() const override { return NumberGetter ? NumberGetter(Data) : 0; }
	[[nodiscard]] const ArrayKey* GetArrayKey() const override { return ArrayKeyGetter ? ArrayKeyGetter(Data) : nullptr; }
	[[nodiscard]] const ForEachContext* GetForEachContext() const override { return ForEachContextGetter ? ForEachContextGetter(Data) : nullptr; }
	[[nodiscard]] const Slice* GetSlice() const override { return SliceGetter ? SliceGetter(Data) : nullptr; }

	[[nodiscard]] bool CanConvertTo(Token_Type toType) const override { return CanConvertToGetter ? CanConvertToGetter(Data, toType) : false; }

	[[nodiscard]] ArrayID GetOwningArrayID() const override { return OwningArrayIDGetter ? OwningArrayIDGetter(Data) : 0; }
	[[nodiscard]] const TokenPair* GetPair() const override { return TokenPairGetter ? TokenPairGetter(Data) : nullptr; }

	[[nodiscard]] ArrayID GetArray() const override { return TokenPairGetter ? TokenPairGetter(Data) : 0; }
};

// If the data pack type you are creating this for (DataPack_t) has overloads for all of the required functions, then this function
// can be used to construct an instance with the appropriate overloads chosen, for partial implementation of the polymorphic interface,
// you will have to set those manually or make a different factory function like this one for it.
template<class DataPack_t>
auto GetScriptTokenBehaviorModel() -> ScriptTokenBehaviorModel<DataPack_t>
{
	return ScriptTokenBehaviorModel<DataPack_t>
	{
		.StringGetter = GetString,
		.BoolGetter = GetBool,
		.FormIdGetter = GetFormId,
		.TESFormGetter = GetTESForm,
		.NumberGetter = GetNumber,
		// Not necessary for this factory, a different data pack type might use them, however.
		//.ArrayKeyGetter = GetArrayKey
		//.ForEachContextGetter = GetForEachContext
		//.SliceGetter = GetSlice
		.CanConvertToGetter = CanConvertTo,
		//.OwningArrayIDGetter = GetOwningArrayID
		//.TokenPairGetter = GetTokenPair
	};
}

// TODO, factory funcs that use the behavior model.
// TODO replace inheriting classes that add custom behavior, possibly with relevant factory funcs.

// slightly less ugly but still cheap polymorphism
struct ScriptToken
{
protected:
	Token_Type	type;
	UInt8		variableType{};
	UInt16		refIdx{};

	struct Value {
		std::string					str;
		union {
			Script::RefVariable		* refVar;
			UInt32					formID;
			double					num;
			TESGlobal				* global;
			Operator				* op;
#if OBLIVION		// run-time only
			ArrayID					arrID;
			ScriptEventList::Var	* var;
#endif
			// compile-time only
			Script::VariableInfo	* varInfo;
			CommandInfo				* cmd;
			ScriptToken				* token;
		};
	} value;
public:
	ScriptToken();
	ScriptToken(Token_Type _type, UInt8 _varType, UInt16 _refIdx);
	ScriptToken(bool boolean);
	ScriptToken(double num);
	ScriptToken(Script::RefVariable* refVar, UInt16 refIdx);
	ScriptToken(Script::VariableInfo* varInfo, UInt16 refIdx, UInt32 varType);
	ScriptToken(CommandInfo* cmdInfo, UInt16 refIdx);
	ScriptToken(std::string str);
	ScriptToken(TESGlobal* global, UInt16 refIdx);
	ScriptToken(Operator* op);
	ScriptToken(UInt32 id, Token_Type asType);		// ArrayID or FormID
	ScriptToken(const ScriptToken& rhs) = delete;	// unimplemented, don't want copy constructor called
#if OBLIVION
	ScriptToken(ScriptEventList::Var* var);
#endif

	Token_Type	ReadFrom(ExpressionEvaluator* context);	// reconstitute param from compiled data, return the type
public:
	virtual	~ScriptToken();

	virtual const char	*			GetString() const;
	virtual UInt32					GetFormID() const;
	virtual TESForm*				GetTESForm() const;
	virtual double					GetNumber() const;
	virtual const ArrayKey *		GetArrayKey() const { return NULL; }
	virtual const ForEachContext *	GetForEachContext() const { return NULL; }
	virtual const Slice *			GetSlice() const { return NULL; }
	virtual bool					GetBool() const;
#if OBLIVION
	virtual ArrayID					GetArray() const;
	ScriptEventList::Var *	GetVar() const;
#endif
	virtual bool			CanConvertTo(Token_Type to) const;	// behavior varies b/w compile/run-time for ambiguous types
	virtual ArrayID			GetOwningArrayID() const { return 0; }
	virtual const ScriptToken  *  GetToken() const { return NULL; }
	virtual const TokenPair	* GetPair() const { return NULL; }

	ScriptToken	*			ToBasicToken() const;		// return clone as one of string, number, array, form
	
	TESGlobal *				GetGlobal() const;
	Operator *				GetOperator() const;
	Script::VariableInfo *	GetVarInfo() const;
	CommandInfo *			GetCommandInfo() const;
	Script::RefVariable*	GetRefVariable() const {return type == kTokenType_Ref ? value.refVar : NULL;}
	UInt16					GetRefIndex() const { return IsGood() ? refIdx : 0; }
	UInt8					GetVariableType() const { return IsVariable() ? variableType : Script::eVarType_Invalid; }

	UInt32					GetActorValue() const;		// kActorVal_XXX or kActorVal_NoActorValue if none
	char					GetAxis() const;			// 'X', 'Y', 'Z', or otherwise -1
	UInt32					GetSex() const;				// 0=male, 1=female, otherwise -1
	UInt32					GetAnimGroup() const;		// TESAnimGroup::kAnimGroup_XXX (kAnimGroup_Max if none)
	EffectSetting *			GetEffectSetting() const;	// from string, effect code, or TESForm*

	bool					Write(ScriptLineBuffer* buf);
	Token_Type				Type() const		{ return type; }

	bool					IsGood() const		{ return type != kTokenType_Invalid;	}
	bool					IsVariable() const	{ return type >= kTokenType_NumericVar && type <= kTokenType_ArrayVar; }
	double					GetNumericRepresentation(bool bFromHex);	// attempts to convert string to number

	static ScriptToken* Read(ExpressionEvaluator* context);

	static ScriptToken* Create(ForEachContext* forEach);
	static ScriptToken* Create(ArrayID arrID, ArrayKey* key);
	static ScriptToken* Create(Slice* slice);
	static ScriptToken* Create(ScriptToken* l, ScriptToken* r);
	static ScriptToken* Create(UInt32 varID, UInt32 lbound, UInt32 ubound);
	static ScriptToken* Create(ArrayElementToken* elem, UInt32 lbound, UInt32 ubound);
};

[[nodiscard]]
inline auto MakeScriptToken(const bool theValue) -> ScriptToken* { return new ScriptToken{ theValue }; }

// for std::string pass-by-value and std::move is recommended per core guidelines
[[nodiscard]]
inline auto MakeScriptToken(std::string theString) -> ScriptToken* { return new ScriptToken{ std::move(theString) }; }

[[nodiscard]]
inline auto MakeScriptToken() -> ScriptToken* { return new ScriptToken{}; }

[[nodiscard]]
inline auto MakeScriptToken(Script::RefVariable* refVar, const UInt16 refIdx) -> ScriptToken* {	return refVar ? new ScriptToken(refVar, refIdx) : nullptr; }

[[nodiscard]]
inline auto MakeScriptToken(Script::VariableInfo* varInfo, const UInt16 refIdx, UInt32 varType) -> ScriptToken* { return varInfo ? new ScriptToken(varInfo, refIdx, varType) : nullptr; }

[[nodiscard]]
inline auto MakeScriptToken(CommandInfo* cmdInfo, UInt16 refIdx) -> ScriptToken* { return cmdInfo ? new ScriptToken(cmdInfo, refIdx) : nullptr; }

[[nodiscard]]
inline auto MakeScriptToken(TESGlobal* global, UInt16 refIdx) -> ScriptToken* { return global ? new ScriptToken(global, refIdx) : nullptr; }

[[nodiscard]]
inline auto MakeScriptToken(Operator* op) -> ScriptToken* { return op ? new ScriptToken(op) : nullptr; }

[[nodiscard]]
inline auto MakeScriptToken(TESForm* form) -> ScriptToken* { return new ScriptToken(form ? form->refID : 0, kTokenType_Form); }

[[nodiscard]]
inline auto MakeScriptTokenForm(const UInt32 formID) -> ScriptToken* { return new ScriptToken(formID, kTokenType_Form); }

[[nodiscard]]
inline auto MakeScriptTokenArray(const ArrayID arrID) -> ScriptToken* { return new ScriptToken(arrID, kTokenType_Array); }



struct SliceToken : public ScriptToken
{
	Slice	slice;

	SliceToken(Slice* _slice);
	virtual const Slice* GetSlice() const { return type == kTokenType_Slice ? &slice : NULL; }
};

struct PairToken : public ScriptToken
{
	TokenPair	pair;

	PairToken(ScriptToken* l, ScriptToken* r);
	virtual const TokenPair* GetPair() const { return type == kTokenType_Pair ? &pair : NULL; }
};

#if OBLIVION

struct ArrayElementToken : public ScriptToken
{
	ArrayKey	key;

	ArrayElementToken(ArrayID arr, ArrayKey* _key);
	virtual const ArrayKey*	GetArrayKey() const { return type == kTokenType_ArrayElement ? &key : NULL; }
	virtual const char*		GetString() const;
	virtual double			GetNumber() const;
	virtual UInt32			GetFormID() const;
	virtual ArrayID			GetArray() const;
	virtual TESForm*		GetTESForm() const;
	virtual bool			GetBool() const;
	virtual bool			CanConvertTo(Token_Type to) const;
	virtual ArrayID			GetOwningArrayID() const { return type == kTokenType_ArrayElement ? value.arrID : 0; }
};

struct ForEachContextToken : public ScriptToken
{
	ForEachContext		context;

	ForEachContextToken(UInt32 srcID, UInt32 iterID, UInt32 varType, ScriptEventList::Var* var);
	virtual const ForEachContext* GetForEachContext() const { return Type() == kTokenType_ForEachContext ? &context : NULL; }
};

struct AssignableStringToken : public ScriptToken
{
	UInt32		lower;
	UInt32		upper;
	std::string	substring;

	AssignableStringToken(UInt32 _id, UInt32 lbound, UInt32 ubound);
	virtual const char* GetString() const { return substring.c_str(); }
	virtual bool Assign(const char* str) = 0;
};

struct AssignableStringVarToken : public AssignableStringToken
{
	AssignableStringVarToken(UInt32 _id, UInt32 lbound, UInt32 ubound);
	virtual bool Assign(const char* str);
};

struct AssignableStringArrayElementToken : public AssignableStringToken
{
	ArrayKey	key;

	AssignableStringArrayElementToken(UInt32 _id, const ArrayKey& _key, UInt32 lbound, UInt32 ubound);
	virtual ArrayID		GetArray() const { return value.arrID; }
	virtual bool Assign(const char* str);
};

#endif

using Op_Eval = ScriptToken* (*)(OperatorType op, ScriptToken* lh, ScriptToken* rh, ExpressionEvaluator* context);

struct OperationRule
{
	Token_Type	lhs;
	Token_Type	rhs;
	Token_Type	result;
	Op_Eval		eval;
	bool		bAsymmetric;	// does order matter? e.g. var := constant legal, constant := var illegal
};

struct Operator
{
	UInt8			precedence;
	char			symbol[3];
	UInt8			numOperands;
	OperatorType	type;
	UInt8			numRules;
	OperationRule	* rules;

	bool Precedes(Operator* op) {
		if (!IsRightAssociative())
			return op->precedence <= precedence;
		else
			return op->precedence < precedence;
	}
	bool IsRightAssociative()	{ return type == kOpType_Assignment || IsUnary() || (type >= kOpType_PlusEquals && type <= kOpType_MinusEquals);	}
	bool IsUnary()	{ return numOperands == 1;	}
	bool IsBinary() { return numOperands == 2;	}
	bool IsOpenBracket() { return (type == kOpType_LeftParen || type == kOpType_LeftBracket); }
	bool IsClosingBracket() { return (type == kOpType_RightParen || type == kOpType_RightBracket); }
	bool IsBracket() { return (IsOpenBracket() || IsClosingBracket()); }
	char GetMatchedBracket() {
		switch (type) {
			case kOpType_LeftBracket :	return ']';
			case kOpType_RightBracket : return '[';
			case kOpType_LeftParen:		return ')';
			case kOpType_RightParen:	return '(';
			default: return 0;
		}
	}
	bool ExpectsStringLiteral() { return type == kOpType_MemberAccess; }

	Token_Type GetResult(Token_Type lhs, Token_Type rhs);	// at compile-time determine type resulting from operation
	ScriptToken* Evaluate(ScriptToken* lhs, ScriptToken* rhs, ExpressionEvaluator* context);	// at run-time, operate on the operands and return result
};
