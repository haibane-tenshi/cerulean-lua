use super::{TypeMismatchOrError, TypeProvider, Value};

pub struct LuaString<T>(pub T);

impl<T, Types> TryFrom<Value<Types>> for LuaString<T>
where
    Types: TypeProvider,
    Types::String: TryInto<Self>,
{
    type Error = TypeMismatchOrError<<Types::String as TryInto<Self>>::Error>;

    fn try_from(value: Value<Types>) -> Result<Self, Self::Error> {
        match value {
            Value::String(t) => {
                let r = t.try_into().map_err(TypeMismatchOrError::Other)?;
                Ok(r)
            }
            value => {
                use super::{Type, TypeMismatchError};

                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                Err(TypeMismatchOrError::TypeMismatch(err))
            }
        }
    }
}

impl<T, Types> From<LuaString<T>> for Value<Types>
where
    Types: TypeProvider,
    T: Into<Types::String>,
{
    fn from(value: LuaString<T>) -> Self {
        let LuaString(value) = value;

        Value::String(value.into())
    }
}
