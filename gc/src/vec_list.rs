#[derive(Debug, Clone)]
pub(crate) struct VecList<T> {
    values: Vec<Place<T>>,
    head: Option<usize>,
}

impl<T> VecList<T> {
    pub(crate) fn with_capacity(capacity: usize) -> Self {
        let mut values = Vec::with_capacity(capacity);

        let iter = (1..capacity)
            .map(|i| Place::NextOpen(Some(i)))
            .chain(std::iter::once(Place::NextOpen(None)));
        values.extend(iter);

        let head = Some(0);

        VecList { values, head }
    }

    fn true_len(&self) -> usize {
        self.values
            .iter()
            .filter(|place| place.is_occupied())
            .count()
    }

    // pub(crate) fn len(&self) -> usize {
    //     self.values.len()
    // }

    pub(crate) fn grow(&mut self) {
        let extra = if self.values.is_empty() {
            10
        } else {
            let true_len = self.true_len();

            if true_len * 4 > self.values.len() * 3 {
                self.values.len() / 4
            } else {
                return;
            }
        };

        let len = self.values.len();
        let iter = (len + 1..len + extra)
            .map(|i| Place::NextOpen(Some(i)))
            .chain(std::iter::once(Place::NextOpen(self.head.take())));

        self.values.reserve(extra);
        self.values.extend(iter);

        self.head = Some(len);
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        self.values.get(index).and_then(Place::as_ref)
    }

    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.values.get_mut(index).and_then(Place::as_mut)
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<usize, T> {
        if let Some(index) = self.head.take() {
            let place = self
                .values
                .get_mut(index)
                .expect("head should point to allocated memory");

            let Place::NextOpen(open) = place else {
                unreachable!()
            };

            self.head = *open;
            *place = Place::Occupied(value);

            Ok(index)
        } else {
            Err(value)
        }
    }

    pub(crate) fn insert(&mut self, value: T) -> usize {
        self.grow();

        let Ok(index) = self.try_insert(value) else {
            unreachable!()
        };

        index
    }

    pub(crate) fn remove(&mut self, index: usize) -> Option<T> {
        let place = self.values.get_mut(index)?;

        if place.is_occupied() {
            let inner = std::mem::replace(place, Place::NextOpen(self.head.take()));

            let Place::Occupied(value) = inner else {
                unreachable!()
            };

            self.head = Some(index);

            Some(value)
        } else {
            None
        }
    }

    pub(crate) fn place_iter(&self) -> impl Iterator<Item = Option<&T>> {
        self.values.iter().map(|place| place.as_ref())
    }

    // pub(crate) fn place_iter_mut(&mut self) -> impl Iterator<Item = Option<&mut T>> {
    //     self.values.iter_mut().map(|place| place.as_mut())
    // }
}

impl<T> Default for VecList<T> {
    fn default() -> Self {
        Self {
            values: Default::default(),
            head: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
enum Place<T> {
    Occupied(T),
    NextOpen(Option<usize>),
}

impl<T> Place<T> {
    fn as_ref(&self) -> Option<&T> {
        match self {
            Place::Occupied(t) => Some(t),
            Place::NextOpen(_) => None,
        }
    }

    fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Place::Occupied(t) => Some(t),
            Place::NextOpen(_) => None,
        }
    }

    fn is_occupied(&self) -> bool {
        match self {
            Place::Occupied(_) => true,
            Place::NextOpen(_) => false,
        }
    }
}
