use bevy_core::Name;
use bevy_ecs::{
    component::Component,
    entity::Entity,
    query::Without,
    system::{Query, SystemParam},
};
use bevy_hierarchy::{Children, Parent};
use std::{
    borrow::Borrow,
    fmt::Display,
    ops::{Deref, DerefMut, Index},
    slice::SliceIndex,
    str::FromStr,
};

// pub struct EntityPathIndex {
//     index: HashMap<(Entity, EntityPathBuf), Entity>,
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityPathBuf {
    path: Vec<PathComponent>,
}

impl EntityPathBuf {
    pub fn new(path: Vec<PathComponent>) -> EntityPathBuf {
        EntityPathBuf { path }
    }
}

impl FromIterator<PathComponent> for EntityPathBuf {
    fn from_iter<T: IntoIterator<Item = PathComponent>>(iter: T) -> Self {
        EntityPathBuf::new(Vec::from_iter(iter))
    }
}

#[derive(Debug)]
pub enum ParseEntityPathError {
    ComponentEmpty,
}
impl Display for ParseEntityPathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseEntityPathError::ComponentEmpty => write!(f, "path component is empty"),
        }
    }
}
impl std::error::Error for ParseEntityPathError {}

impl FromStr for EntityPathBuf {
    type Err = ParseEntityPathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(EntityPathBuf::new(vec![]));
        }
        s.split('/')
            .map(|a| match a {
                ".." => Ok(PathComponent::Parent),
                "." => Ok(PathComponent::Current),
                "" => Err(ParseEntityPathError::ComponentEmpty),
                name => Ok(PathComponent::Child(Name::new(name.to_owned()))),
            })
            .collect::<Result<_, _>>()
    }
}

impl Deref for EntityPathBuf {
    type Target = EntityPath;

    fn deref(&self) -> &Self::Target {
        EntityPath::from_inner(&self.path)
    }
}

impl DerefMut for EntityPathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        EntityPath::from_inner_mut(&mut self.path)
    }
}

impl Borrow<EntityPath> for EntityPathBuf {
    fn borrow(&self) -> &EntityPath {
        EntityPath::from_inner(&self.path)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct EntityPath {
    path: [PathComponent],
}

impl EntityPath {
    pub fn new<P: AsRef<[PathComponent]>>(path: &P) -> &EntityPath {
        EntityPath::from_inner(path.as_ref())
    }

    const fn from_inner(path: &[PathComponent]) -> &EntityPath {
        // SAFETY: EntityPath is just a wrapper over `[PathComponent]`
        unsafe { &*(path as *const [PathComponent] as *const EntityPath) }
    }

    fn from_inner_mut(path: &mut [PathComponent]) -> &mut EntityPath {
        // SAFETY: EntityPath is just a wrapper over `[PathComponent]`
        unsafe { &mut *(path as *mut [PathComponent] as *mut EntityPath) }
    }

    pub const fn components(&self) -> &[PathComponent] {
        &self.path
    }
}

impl Display for EntityPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut components = self.components().iter();

        if let Some(c) = components.next() {
            match c {
                PathComponent::Child(name) => write!(f, "{}", name)?,
                PathComponent::Parent => write!(f, "..")?,
                PathComponent::Current => write!(f, ".")?,
            }
        };

        for c in components {
            write!(f, "/")?;
            match c {
                PathComponent::Child(name) => write!(f, "{}", name)?,
                PathComponent::Parent => write!(f, "..")?,
                PathComponent::Current => write!(f, ".")?,
            }
        }
        Ok(())
    }
}

impl AsRef<EntityPath> for [PathComponent] {
    fn as_ref(&self) -> &EntityPath {
        EntityPath::from_inner(self)
    }
}

impl ToOwned for EntityPath {
    type Owned = EntityPathBuf;

    fn to_owned(&self) -> Self::Owned {
        EntityPathBuf::new(self.path.to_owned())
    }
}

// impl<I: SliceIndex<[PathComponent], Output = PathComponent>> Index<I> for EntityPath {
//     type Output = I::Output;

//     fn index(&self, index: I) -> &Self::Output {
//         Index::index(self.components(), index)
//     }
// }

impl<I: SliceIndex<[PathComponent], Output = [PathComponent]>> Index<I> for EntityPath {
    type Output = EntityPath;

    fn index(&self, index: I) -> &Self::Output {
        EntityPath::from_inner(Index::index(self.components(), index))
    }
}

#[derive(Debug, Component)]
struct UniqueName;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathComponent {
    Child(Name),
    Parent,
    Current,
}

/// Access as a parameter in systems
/// Not a good naming yet
#[derive(SystemParam)]
pub struct EntityPathParam<'w, 's> {
    q_parent: Query<'w, 's, &'static Parent>,
    q_children: Query<'w, 's, &'static Children>,
    q_name: Query<'w, 's, &'static Name>,
    q_orphan_name: Query<'w, 's, (Entity, &'static Name), Without<Parent>>,
}

impl<'w, 's> EntityPathParam<'w, 's> {
    pub fn root(&self, path: &EntityPath) -> Option<Entity> {
        match path.components().split_first() {
            Some((first, path)) => match first {
                PathComponent::Child(expect_name) => {
                    let (first, _) = self
                        .q_orphan_name
                        .iter()
                        .find(|(_, name)| **name == *expect_name)?;
                    self.entity(first, path.as_ref())
                }
                PathComponent::Parent => None,
                PathComponent::Current => self.root(path.as_ref()),
            },
            None => None,
        }
    }
    pub fn scene_root(&self, path: &EntityPath) -> Option<Entity> {
        todo!()
    }
    pub fn entity(&self, entity: Entity, path: &EntityPath) -> Option<Entity> {
        let mut cur_entity = entity;
        for segment in path.path.iter() {
            match segment {
                PathComponent::Child(expect_name) => {
                    let children = self.q_children.get(cur_entity).ok()?;
                    let found_child = children.iter().find(|&&child| {
                        if let Ok(child_name) = self.q_name.get(child) {
                            child_name == expect_name
                        } else {
                            false
                        }
                    });
                    if let Some(child) = found_child {
                        cur_entity = *child;
                    } else {
                        return None;
                    }
                }
                PathComponent::Parent => {
                    let Ok(parent) = self.q_parent.get(cur_entity) else {
                        return None;
                    };
                    cur_entity = parent.get();
                }
                PathComponent::Current => {}
            }
        }
        Some(cur_entity)
    }
}

#[cfg(test)]
mod test {
    use bevy_ecs::{system::RunSystemOnce, world::World};
    use bevy_hierarchy::BuildWorldChildren;

    use super::*;

    #[test]
    fn parse() {
        use PathComponent::*;
        assert_eq!(
            EntityPathBuf {
                path: vec![
                    Child(Name::new("a")),
                    Child(Name::new("b")),
                    Child(Name::new("c")),
                    Child(Name::new("d")),
                ]
            },
            "a/b/c/d".parse().unwrap(),
        );
        assert_eq!(
            EntityPathBuf {
                path: vec![
                    Child(Name::new("a")),
                    Parent,
                    Child(Name::new("b")),
                    Current,
                    Child(Name::new("c")),
                    Child(Name::new("d")),
                ]
            },
            "a/../b/./c/d".parse().unwrap(),
        );
        assert_eq!(
            EntityPathBuf {
                path: vec![Current, Current, Child(Name::new("a")),]
            },
            "././a".parse().unwrap(),
        );
        assert!(matches!(
            "a//d".parse::<EntityPathBuf>().err().unwrap(),
            ParseEntityPathError::ComponentEmpty,
        ));
    }

    #[test]
    fn query() {
        fn e(s: &str) -> EntityPathBuf {
            s.parse().unwrap()
        }
        let mut world = World::new();
        let mut entity_b = Entity::PLACEHOLDER;
        let mut entity_c = Entity::PLACEHOLDER;
        let entity_a = world
            .spawn(Name::new("a"))
            .with_children(|c| {
                entity_b = c
                    .spawn(Name::new("b"))
                    .with_children(|c| {
                        entity_c = c.spawn(Name::new("c")).id();
                    })
                    .id();
            })
            .id();

        world.run_system_once(move |path: EntityPathParam| {
            assert_eq!(path.entity(entity_a, &e("")), Some(entity_a));

            assert_eq!(path.entity(entity_a, &e(".")), Some(entity_a));

            assert_eq!(path.entity(entity_a, &e("././.")), Some(entity_a));

            assert_eq!(path.entity(entity_a, &e("b")), Some(entity_b));

            assert_eq!(path.entity(entity_a, &e("ah")), None);

            assert_eq!(path.entity(entity_a, &e("b/c")), Some(entity_c));

            assert_eq!(path.entity(entity_a, &e("b/..")), Some(entity_a));

            assert_eq!(path.entity(entity_a, &e("b/../b/c")), Some(entity_c));

            assert_eq!(path.entity(entity_a, &e("..")), None);

            assert_eq!(path.entity(entity_c, &e("..")), Some(entity_b));

            assert_eq!(path.root(&e("")), None);

            assert_eq!(path.root(&e("..")), None);

            assert_eq!(path.root(&e("././a")), Some(entity_a));

            assert_eq!(path.root(&e("a")), Some(entity_a));

            assert_eq!(path.root(&e("a/b")), Some(entity_b));

            assert_eq!(path.root(&e("a/b/c")), Some(entity_c));
        });
    }
}
