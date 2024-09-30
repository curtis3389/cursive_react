use cursive::view::Nameable;
use cursive::views::{BoxedView, Button, Dialog, DummyView, LinearLayout, NamedView};
use cursive::{CbSink, Cursive};
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::{Arc, RwLock};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ElementType {
    Button,
    Component(fn(&mut React) -> Option<Element>),
    Dialog,
    VerticalLayout,
}

#[derive(Clone)]
pub struct Props {
    pub children: Vec<Element>,
    pub title: String,
    pub on_submit: Arc<dyn Fn(&mut Cursive) + Send + Sync>,
    pub key: Option<String>,
}

impl Props {
    pub fn new() -> Self {
        Self {
            children: vec![],
            title: String::new(),
            on_submit: Arc::new(|_| {}),
            key: None,
        }
    }
}

impl Debug for Props {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?}:{:?}:{:?}",
            self.title, self.key, self.children
        ))
    }
}

impl Default for Props {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    pub element_type: ElementType,
    pub props: Props,
}

impl Element {
    pub fn new(element_type: ElementType, props: Props) -> Self {
        Self {
            element_type,
            props,
        }
    }

    pub fn resolve(&self, react: &mut React, name: String) -> Option<Fiber> {
        let element = match self.element_type {
            ElementType::Component(component) => {
                react.element_key.clone_from(&name);
                react.state_index = 0;
                component(react)
            }
            _ => Some(self.clone()),
        };
        element.map(|element| {
            let children = element
                .props
                .children
                .iter()
                .enumerate()
                .filter_map(|(i, e)| {
                    e.resolve(
                        react,
                        format!("{}.{}", name, e.props.key.clone().unwrap_or(i.to_string())),
                    )
                })
                .collect();
            Fiber {
                element,
                children,
                name,
            }
        })
    }
}

#[derive(Clone, Debug)]
pub struct Fiber {
    pub element: Element,
    pub children: Vec<Fiber>,
    pub name: String,
}

pub struct React {
    element_key: String,
    state_index: usize,
    state: Arc<RwLock<HashMap<String, Box<dyn std::any::Any + Send + Sync>>>>,
    root: Option<Element>,
    old_root: Option<Fiber>,
    cb_sink: Option<CbSink>,
}

impl React {
    pub fn new() -> Self {
        Self {
            element_key: String::new(),
            state_index: 0,
            state: Arc::new(RwLock::new(HashMap::new())),
            root: None,
            old_root: None,
            cb_sink: None,
        }
    }

    pub fn render(mut self, element: Element) {
        let mut s = cursive::default();
        self.cb_sink = Some(s.cb_sink().clone());
        self.root = Some(element.clone());
        self.render_root(s.borrow_mut(), &element);
        s.set_user_data(self);
        s.run();
    }

    pub fn render_root(&mut self, s: &mut Cursive, element: &Element) {
        let old_root = self.old_root.clone();
        let new_root = element.resolve(self, String::from("root"));

        match &new_root {
            Some(new_root) => {
                let new_root_copy = new_root.clone();
                let result = s.call_on_name("root", move |root_view: &mut NamedView<BoxedView>| {
                    let old_root = old_root.unwrap();
                    if old_root.element.element_type == new_root_copy.element.element_type {
                        Self::update_view(root_view, &old_root, &new_root_copy);
                        false
                    } else {
                        true
                    }
                });
                match result {
                    Some(true) => {
                        // need to swap
                        s.pop_layer();
                        s.add_layer(Self::create_view(new_root));
                    }
                    Some(false) => {
                        // updated, nothing else to do
                    }
                    None => {
                        // no existing root; create a new one
                        s.add_layer(Self::create_view(new_root));
                    }
                }
            }
            None => {
                // no new root, pop top layer
                // TODO: if we have multiple layers, we gotta pop all
                s.pop_layer();
            }
        }

        self.old_root = new_root;
    }

    fn create_view(fiber: &Fiber) -> NamedView<BoxedView> {
        match fiber.element.element_type {
            ElementType::Button => {
                let on_submit = fiber.element.props.on_submit.clone();
                BoxedView::boxed(Button::new(fiber.element.props.title.clone(), move |s| {
                    (on_submit)(s)
                }))
            }
            ElementType::Component(_) => {
                panic!("Can't create view for component! How did you even get here?!")
            }
            ElementType::Dialog => {
                let content = fiber
                    .children
                    .first()
                    .map(Self::create_view)
                    .unwrap_or(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                BoxedView::boxed(Dialog::around(content).title(fiber.element.props.title.clone()))
            }
            ElementType::VerticalLayout => {
                let mut layout = LinearLayout::vertical();
                for child_fiber in &fiber.children {
                    layout.add_child(Self::create_view(child_fiber));
                }
                BoxedView::boxed(layout)
            }
        }
        .with_name(fiber.name.clone())
    }

    fn update_view(view: &mut NamedView<BoxedView>, old_fiber: &Fiber, new_fiber: &Fiber) {
        // this is where we loop over children and recurse
        match old_fiber.element.element_type {
            ElementType::Button => {
                let mut button = view.get_mut();
                let button = button.get_mut::<Button>().unwrap();
                button.set_label(new_fiber.element.props.title.clone());
                let on_submit = new_fiber.element.props.on_submit.clone();
                button.set_callback(move |s| (on_submit)(s));
            }
            ElementType::Dialog => {
                let mut dialog = view.get_mut();
                let dialog = dialog.get_mut::<Dialog>().unwrap();
                dialog.set_title(new_fiber.element.props.title.clone());
                if old_fiber.element.element_type == new_fiber.element.element_type {
                    let child_view = dialog
                        .get_content_mut()
                        .downcast_mut::<NamedView<BoxedView>>()
                        .unwrap();
                    let old_child_fiber = old_fiber.children.first();
                    let new_child_fiber = new_fiber.children.first();
                    match (old_child_fiber, new_child_fiber) {
                        (Some(old_fiber), Some(new_fiber)) => {
                            if old_fiber.element.element_type == new_fiber.element.element_type {
                                Self::update_view(child_view, old_fiber, new_fiber);
                            } else {
                                dialog.set_content(Self::create_view(new_fiber));
                            }
                        }
                        (None, None) => {}
                        (None, Some(new_fiber)) => {
                            dialog.set_content(Self::create_view(new_fiber));
                        }
                        (Some(_), None) => {
                            dialog
                                .set_content(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                        }
                    }
                }
            }
            ElementType::Component(_) => {}
            ElementType::VerticalLayout => {
                let old_fibers = &old_fiber.children;
                let new_fibers = &new_fiber.children;
                let mut layout = view.get_mut();
                let layout = layout.get_mut::<LinearLayout>().unwrap();
                let mut to_remove: Vec<usize> = vec![];
                for i in 0..layout.len() {
                    let child_view = layout
                        .get_child_mut(i)
                        .unwrap()
                        .downcast_mut::<NamedView<BoxedView>>()
                        .unwrap();
                    if let Some(old_child_fiber) = old_fibers
                        .iter()
                        .find(|f| f.name.as_str() == child_view.name())
                    {
                        if let Some(new_child_fiber) = new_fibers
                            .iter()
                            .find(|f| f.name.as_str() == child_view.name())
                        {
                            if old_child_fiber.element.element_type
                                == new_child_fiber.element.element_type
                            {
                                // we have a matching old and new fiber of the same type, need to update
                                Self::update_view(child_view, old_child_fiber, new_child_fiber);
                            } else {
                                // we have an old and new fiber of diff types, need to swap
                                to_remove.push(i);
                                let new_view = Self::create_view(new_child_fiber);
                                layout.add_child(new_view);
                            }
                        } else {
                            // we have an old fiber but not a new one, need to remove
                            to_remove.push(i);
                        }
                    } else {
                        // no old fiber for existing view, need to remove
                        to_remove.push(i);
                    }
                }

                for i in to_remove {
                    layout.remove_child(i);
                }

                // for each new child not in old fibers, add
                // we can blindly add because we've removed old views w/o an old fiber
                for new_child_fiber in new_fibers {
                    if !old_fibers.iter().any(|f| f.name == new_child_fiber.name) {
                        let new_view = Self::create_view(new_fiber);
                        layout.add_child(new_view);
                    }
                }
            }
        }
    }

    pub fn use_state<T>(&mut self, initial_value: T) -> (T, Arc<dyn Fn(T) + Send + Sync>)
    where
        T: Clone + Send + Sync + 'static,
    {
        let cb_sink = self.cb_sink.as_ref().unwrap().clone();
        let key = format!("{}.{}", self.element_key, self.state_index);
        self.state_index += 1;
        let state = Arc::clone(&self.state);

        // Set initial value if it doesn't exist
        {
            state
                .as_ref()
                .write()
                .unwrap()
                .entry(key.clone())
                .or_insert_with(|| {
                    Box::new(initial_value.clone()) as Box<dyn std::any::Any + Send + Sync>
                });
        }

        let set_state = {
            let state = Arc::clone(&self.state);
            let key = key.clone();
            Arc::new(move |new_value: T| {
                state
                    .as_ref()
                    .write()
                    .unwrap()
                    .insert(key.clone(), Box::new(new_value.clone())); // Store the new value
                cb_sink
                    .send(Box::new(|s| {
                        let mut react = { s.take_user_data::<React>().unwrap() };
                        let root = react.root.clone().unwrap();
                        react.render_root(s, &root);
                        s.set_user_data(react);
                    }))
                    .unwrap();
            })
        };

        // Retrieve the current value
        let current_value = state
            .read()
            .unwrap()
            .get(&key)
            .and_then(|value| value.downcast_ref::<T>().cloned())
            .unwrap_or_else(|| initial_value.clone());

        (current_value, set_state)
    }
}

impl Default for React {
    fn default() -> Self {
        Self::new()
    }
}

fn main() {
    let react = React::new();
    react.render(Element::new(ElementType::Component(app), Props::new()));
}

fn app(_react: &mut React) -> Option<Element> {
    Some(Element::new(
        ElementType::Dialog,
        Props {
            title: String::from("Test Title"),
            children: vec![Element::new(ElementType::Component(counter), Props::new())],
            on_submit: Arc::new(|_| {}),
            key: None,
        },
    ))
}

fn counter(react: &mut React) -> Option<Element> {
    let (count, set_count) = react.use_state(0);
    Some(Element::new(
        ElementType::Button,
        Props {
            title: format!("{}", count),
            children: vec![],
            on_submit: Arc::new(move |_| set_count(count + 1)),
            key: None,
        },
    ))
}
