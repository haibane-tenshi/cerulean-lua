use std::ops::Range;

#[derive(Debug, Clone)]
pub(crate) struct VecList<T, G> {
    values: Vec<Place<T, G>>,
    head: Option<usize>,
}

impl<T, G> VecList<T, G> {
    fn count(&self) -> usize {
        self.values
            .iter()
            .filter(|place| place.is_occupied())
            .count()
    }
}

impl<T, G> VecList<T, G>
where
    G: GenTag,
{
    pub(crate) fn with_capacity(capacity: usize) -> Self {
        let mut values = Vec::with_capacity(capacity);
        let (start, iter) = chain(0..capacity, None);
        values.extend(iter);

        let head = Some(start);

        VecList { values, head }
    }

    pub(crate) fn grow(&mut self) {
        let extra = if self.values.is_empty() {
            10
        } else {
            let true_len = self.count();

            if true_len * 4 > self.values.len() * 3 {
                self.values.len() / 4
            } else {
                return;
            }
        };

        debug_assert!(extra > 0);

        let len = self.values.len();
        let (start, iter) = chain(len..len + extra, self.head);
        self.values.reserve(extra);
        self.values.extend(iter);

        self.head = Some(start);
    }

    pub(crate) fn get(&self, index: usize, generation: G) -> Option<&T> {
        let (value, g) = self.values.get(index)?.as_ref()?;
        (g == generation).then_some(value)
    }

    pub(crate) fn get_mut(&mut self, index: usize, generation: G) -> Option<&mut T> {
        let (value, g) = self.values.get_mut(index)?.as_mut()?;
        (g == generation).then_some(value)
    }

    pub(crate) fn get_untagged(&self, index: usize) -> Option<&T> {
        self.values.get(index)?.value_ref()
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<(usize, G), T> {
        let Some(index) = self.head.take() else {
            return Err(value);
        };

        let place = self
            .values
            .get_mut(index)
            .expect("head should point to allocated memory");

        let Ok((next_head, gen)) = place.put(value) else {
            unreachable!()
        };

        self.head = next_head;

        Ok((index, gen))
    }

    pub(crate) fn insert(&mut self, value: T) -> (usize, G) {
        self.grow();

        let Ok(index) = self.try_insert(value) else {
            unreachable!()
        };

        index
    }

    pub(crate) fn remove(&mut self, index: usize) -> Option<T> {
        let place = self.values.get_mut(index)?;
        let r = place.remove(self.head);

        if place.is_open() {
            self.head = Some(index);
        }

        r
    }

    pub(crate) fn iter_occupied(&self) -> impl Iterator<Item = Option<&T>> {
        self.values.iter().map(|place| place.value_ref())
    }

    // pub(crate) fn place_iter_mut(&mut self) -> impl Iterator<Item = Option<&mut T>> {
    //     self.values.iter_mut().map(|place| place.as_mut())
    // }
}

impl<T, G> Default for VecList<T, G> {
    fn default() -> Self {
        Self {
            values: Default::default(),
            head: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
enum Place<T, G> {
    Occupied {
        value: T,
        gen: G,
    },
    Open {
        next_open: Option<usize>,
        next_gen: G,
    },
    Dead,
}

impl<T, G> Place<T, G>
where
    G: GenTag,
{
    fn as_ref(&self) -> Option<(&T, G)> {
        match self {
            Place::Occupied { value, gen } => Some((value, *gen)),
            Place::Open { .. } => None,
            Place::Dead => None,
        }
    }

    fn as_mut(&mut self) -> Option<(&mut T, G)> {
        match self {
            Place::Occupied { value, gen } => Some((value, *gen)),
            Place::Open { .. } => None,
            Place::Dead => None,
        }
    }

    fn value_ref(&self) -> Option<&T> {
        self.as_ref().map(|(value, _)| value)
    }

    fn put(&mut self, value: T) -> Result<(Option<usize>, G), T> {
        let Place::Open {
            next_open,
            next_gen,
        } = self
        else {
            return Err(value);
        };

        let gen = *next_gen;
        let next_open = *next_open;

        *self = Place::Occupied { value, gen };

        Ok((next_open, gen))
    }

    fn remove(&mut self, next_open: Option<usize>) -> Option<T> {
        let temp = std::mem::replace(self, Place::Dead);

        let Place::Occupied { value, gen } = temp else {
            let _ = std::mem::replace(self, temp);
            return None;
        };

        *self = match gen.next() {
            Some(next_gen) => Place::Open {
                next_open,
                next_gen,
            },
            None => Place::Dead,
        };

        Some(value)
    }
}

impl<T, G> Place<T, G> {
    fn is_occupied(&self) -> bool {
        match self {
            Place::Occupied { .. } => true,
            Place::Open { .. } => false,
            Place::Dead => false,
        }
    }

    fn is_open(&self) -> bool {
        match self {
            Place::Occupied { .. } => false,
            Place::Open { .. } => true,
            Place::Dead => false,
        }
    }
}

pub(crate) trait GenTag: Copy + Eq + Sized {
    fn new() -> Self;
    fn next(self) -> Option<Self>;
}

impl GenTag for () {
    fn new() -> Self {}

    fn next(self) -> Option<Self> {
        Some(())
    }
}

fn chain<T, G>(
    range: Range<usize>,
    last: Option<usize>,
) -> (usize, impl Iterator<Item = Place<T, G>>)
where
    G: GenTag,
{
    debug_assert!(!range.is_empty());

    let start = range.start;
    let end = range.end;

    let iter = (start + 1..end)
        .map(|i| Place::Open {
            next_open: Some(i),
            next_gen: G::new(),
        })
        .chain(std::iter::once(Place::Open {
            next_open: last,
            next_gen: G::new(),
        }));

    (start, iter)
}
