use cursive::align::HAlign;
use cursive::menu::Tree;
use cursive::view::{Nameable, Resizable};
use cursive::views::{
    BoxedView, Button, Dialog, DummyView, HideableView, LayerPosition, LinearLayout, Menubar,
    NamedView, Panel, ResizedView, StackView,
};
use cursive::{CbSink, Cursive};
use cursive_table_view::{TableView, TableViewItem};
use std::any::Any;
use std::borrow::BorrowMut;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::{Arc, RwLock};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Column {
    pub name: &'static str,
    pub align: Option<HAlign>,
    pub cmp: Option<fn(&Row, &Row, Column) -> Ordering>,
    pub width: Option<usize>,
}

impl Column {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            align: None,
            cmp: None,
            width: None,
        }
    }

    pub fn align_right(mut self) -> Self {
        self.align = Some(HAlign::Right);
        self
    }

    pub fn width(mut self, width: usize) -> Self {
        self.width = Some(width);
        self
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Row {
    data: HashMap<&'static str, String>,
    id: String,
}

impl Row {
    pub fn new(id: &str, data: HashMap<&'static str, String>) -> Self {
        Self {
            data,
            id: String::from(id),
        }
    }
}

impl TableViewItem<Column> for Row {
    fn to_column(&self, column: Column) -> String {
        self.data.get(column.name).unwrap().clone()
    }

    fn cmp(&self, other: &Self, column: Column) -> Ordering
    where
        Self: Sized,
    {
        if let Some(compare_function) = column.cmp {
            compare_function(self, other, column)
        } else {
            let s = self.data.get(column.name).unwrap();
            let o = other.data.get(column.name).unwrap();
            s.cmp(o)
        }
    }
}

type ComponentFn = dyn Fn(&mut React, &dyn Any) -> Option<Element> + Send + Sync;

#[derive(Clone)]
pub enum ElementType {
    Button,
    Component(usize, Arc<ComponentFn>),
    Dialog,
    FullscreenLayer,
    Hideable,
    HorizontalLayout,
    Layer,
    Leaf,
    Menubar,
    Panel,
    Resized,
    Stack,
    Table,
    Tree,
    VerticalLayout,
}

impl PartialEq for ElementType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ElementType::Button, ElementType::Button) => true,
            (ElementType::Component(s, _), ElementType::Component(o, _)) => s == o,
            (ElementType::Dialog, ElementType::Dialog) => true,
            (ElementType::FullscreenLayer, ElementType::FullscreenLayer) => true,
            (ElementType::Hideable, ElementType::Hideable) => true,
            (ElementType::HorizontalLayout, ElementType::HorizontalLayout) => true,
            (ElementType::Layer, ElementType::Layer) => true,
            (ElementType::Leaf, ElementType::Leaf) => true,
            (ElementType::Menubar, ElementType::Menubar) => true,
            (ElementType::Panel, ElementType::Panel) => true,
            (ElementType::Resized, ElementType::Resized) => true,
            (ElementType::Stack, ElementType::Stack) => true,
            (ElementType::Table, ElementType::Table) => true,
            (ElementType::Tree, ElementType::Tree) => true,
            (ElementType::VerticalLayout, ElementType::VerticalLayout) => true,
            _ => false,
        }
    }
}

impl Eq for ElementType {}

pub fn button(title: String, callback: Arc<Callback>) -> Element {
    Element::new(
        ElementType::Button,
        Props::new().title(title).on_submit(callback),
    )
}

pub fn component(function: fn(&mut React) -> Option<Element>) -> Element {
    let id = function as usize;
    let function = Arc::new(move |react: &mut React, _: &dyn Any| function(react));
    Element::new(ElementType::Component(id, function), Props::new())
}

pub fn dialog(title: &str, children: Vec<Element>) -> Element {
    Element::new(
        ElementType::Dialog,
        Props::new().title(String::from(title)).children(children),
    )
}

pub fn fullscreen_layer(element: Element) -> Element {
    Element::new(
        ElementType::FullscreenLayer,
        Props::new().children(vec![element]),
    )
}

pub fn hideable(element: Element) -> Element {
    Element::new(ElementType::Hideable, Props::new().children(vec![element]))
}

pub fn horizontal_layout(children: Vec<Element>) -> Element {
    Element::new(
        ElementType::HorizontalLayout,
        Props::new().children(children),
    )
}

pub fn layer(element: Element) -> Element {
    Element::new(ElementType::Layer, Props::new().children(vec![element]))
}

pub fn menubar(children: Vec<Element>) -> Element {
    Element::new(ElementType::Menubar, Props::new().children(children))
}

pub fn panel(element: Element) -> Element {
    Element::new(ElementType::Panel, Props::new().children(vec![element]))
}

pub fn props_component<P>(function: fn(&mut React, &P) -> Option<Element>, props: P) -> Element
where
    P: 'static + Send + Sync,
{
    let id = function as usize;
    let function = Arc::new(move |react: &mut React, v: &dyn Any| {
        let v = v.downcast_ref::<P>().unwrap();
        function(react, v)
    });
    Element::new(
        ElementType::Component(id, function),
        Props::new().component_props(props),
    )
}

pub fn resized(child: Element) -> Element {
    Element::new(ElementType::Resized, Props::new().children(vec![child]))
}

pub fn tree(label: &str, items: Vec<Element>) -> Element {
    Element::new(
        ElementType::Tree,
        Props::new().children(items).title(label.to_string()),
    )
}

pub fn leaf(label: &str, callback: Arc<Callback>) -> Element {
    Element::new(
        ElementType::Leaf,
        Props::new().on_submit(callback).title(label.to_string()),
    )
}

pub fn stack(children: Vec<Element>) -> Element {
    Element::new(ElementType::Stack, Props::new().children(children))
}

pub fn table(
    columns: Vec<Column>,
    rows: Vec<Row>,
    on_select: Arc<Callback>,
    on_submit: Arc<Callback>,
) -> Element {
    Element::new(
        ElementType::Table,
        Props::new()
            .columns(columns)
            .rows(rows)
            .on_select(on_select)
            .on_submit(on_submit),
    )
}

pub fn vertical_layout(children: Vec<Element>) -> Element {
    Element::new(ElementType::VerticalLayout, Props::new().children(children))
}

pub type Callback = dyn Fn(&mut Cursive) + Send + Sync;

#[derive(Clone)]
pub struct Props {
    pub children: Vec<Element>,
    pub columns: Option<Vec<Column>>,
    pub component_props: Option<Arc<dyn Any + Send + Sync>>,
    pub key: Option<String>,
    pub on_select: Option<Arc<Callback>>,
    pub on_submit: Option<Arc<Callback>>,
    pub rows: Option<Vec<Row>>,
    pub title: Option<String>,
}

impl Props {
    pub fn new() -> Self {
        Self {
            children: vec![],
            columns: None,
            component_props: None,
            key: None,
            on_select: None,
            on_submit: None,
            rows: None,
            title: None,
        }
    }

    pub fn children(mut self, children: Vec<Element>) -> Self {
        self.children = children;
        self
    }

    pub fn columns(mut self, columns: Vec<Column>) -> Self {
        self.columns = Some(columns);
        self
    }

    pub fn component_props<P>(mut self, props: P) -> Self
    where
        P: 'static + Send + Sync,
    {
        self.component_props = Some(Arc::new(props));
        self
    }

    pub fn key(mut self, key: String) -> Self {
        self.key = Some(key);
        self
    }

    pub fn on_select(mut self, on_select: Arc<Callback>) -> Self {
        self.on_select = Some(on_select);
        self
    }

    pub fn on_submit(mut self, on_submit: Arc<Callback>) -> Self {
        self.on_submit = Some(on_submit);
        self
    }

    pub fn rows(mut self, rows: Vec<Row>) -> Self {
        self.rows = Some(rows);
        self
    }

    pub fn title(mut self, title: String) -> Self {
        self.title = Some(title);
        self
    }
}

impl Default for Props {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
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
        let element = match &self.element_type {
            ElementType::Component(_, component) => {
                react.element_key.clone_from(&name);
                react.state_index = 0;
                match &self.props.component_props {
                    Some(props) => component(react, props.as_ref()),
                    None => component(react, &()),
                }
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

pub enum FiberComparison {
    Added,
    None,
    Removed,
    Replaced,
    Updated,
}

pub trait OptionFiberExtensions {
    fn compare(&self, other: &Option<&Fiber>) -> FiberComparison;
}

impl OptionFiberExtensions for Option<&Fiber> {
    fn compare(&self, other: &Option<&Fiber>) -> FiberComparison {
        match (self, other) {
            (Some(old_fiber), Some(new_fiber)) => {
                if old_fiber.element.element_type == new_fiber.element.element_type {
                    FiberComparison::Updated
                } else {
                    FiberComparison::Replaced
                }
            }
            (None, None) => FiberComparison::None,
            (None, Some(_)) => FiberComparison::Added,
            (Some(_), None) => FiberComparison::Removed,
        }
    }
}

#[derive(Clone)]
pub struct Fiber {
    pub element: Element,
    pub children: Vec<Fiber>,
    pub name: String,
}

type StateValue = dyn std::any::Any + Send + Sync;

pub struct React {
    element_key: String,
    state_index: usize,
    state: Arc<RwLock<HashMap<String, Box<StateValue>>>>,
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
                        s.add_fullscreen_layer(Self::create_view(new_root));
                    }
                    Some(false) => {
                        // updated, nothing else to do
                    }
                    None => {
                        // no existing root; create a new one
                        s.add_fullscreen_layer(Self::create_view(new_root));
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
                let on_submit = fiber
                    .element
                    .props
                    .on_submit
                    .clone()
                    .unwrap_or(Arc::new(Cursive::noop));
                BoxedView::boxed(Button::new(
                    fiber.element.props.title.clone().unwrap_or_default(),
                    move |s| (on_submit)(s),
                ))
            }
            ElementType::Component(_, _) => {
                panic!("Can't create view for component! How did you even get here?!")
            }
            ElementType::Dialog => {
                let content = fiber
                    .children
                    .first()
                    .map(Self::create_view)
                    .unwrap_or(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                BoxedView::boxed(
                    Dialog::around(content)
                        .title(fiber.element.props.title.clone().unwrap_or_default()),
                )
            }
            ElementType::Hideable => {
                let content = fiber
                    .children
                    .first()
                    .map(Self::create_view)
                    .unwrap_or(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                BoxedView::boxed(HideableView::new(content))
            }
            ElementType::HorizontalLayout => {
                let mut layout = LinearLayout::horizontal();
                for child_fiber in &fiber.children {
                    layout.add_child(Self::create_view(child_fiber));
                }
                BoxedView::boxed(layout)
            }
            ElementType::FullscreenLayer | ElementType::Layer => {
                panic!("Can't render a layer directly! Layers must be children of stacks!")
            }
            ElementType::Menubar => {
                fn to_tree(fibers: &Vec<Fiber>) -> Tree {
                    let mut tree = Tree::new();
                    for fiber in fibers {
                        match fiber.element.element_type {
                            ElementType::Leaf => {
                                let on_submit = fiber
                                    .element
                                    .props
                                    .on_submit
                                    .clone()
                                    .unwrap_or(Arc::new(Cursive::noop));
                                tree.add_leaf(
                                    fiber
                                        .element
                                        .props
                                        .title
                                        .clone()
                                        .unwrap_or("<blank>".to_string()),
                                    move |s| (on_submit)(s),
                                );
                            }
                            ElementType::Tree => {
                                tree.add_subtree(
                                    fiber
                                        .element
                                        .props
                                        .title
                                        .clone()
                                        .unwrap_or("<blank>".to_string()),
                                    to_tree(&fiber.children),
                                );
                            }
                            _ => panic!(),
                        }
                    }
                    tree
                }

                let mut menubar = Menubar::new();
                for child in &fiber.children {
                    match child.element.element_type {
                        ElementType::Leaf => {
                            let on_submit = child
                                .element
                                .props
                                .on_submit
                                .clone()
                                .unwrap_or(Arc::new(Cursive::noop));
                            menubar.add_leaf(
                                child
                                    .element
                                    .props
                                    .title
                                    .clone()
                                    .unwrap_or("<blank>".to_string()),
                                move |s| (on_submit)(s),
                            );
                        }
                        ElementType::Tree => {
                            menubar.add_subtree(
                                child
                                    .element
                                    .props
                                    .title
                                    .clone()
                                    .unwrap_or("<blank>".to_string()),
                                to_tree(&child.children),
                            );
                        }
                        _ => panic!(),
                    }
                }
                BoxedView::boxed(menubar)
            }
            ElementType::Panel => {
                let content = fiber
                    .children
                    .first()
                    .map(Self::create_view)
                    .unwrap_or(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                BoxedView::boxed(Panel::new(content))
            }
            ElementType::Resized => {
                let content = fiber
                    .children
                    .first()
                    .map(Self::create_view)
                    .unwrap_or(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                BoxedView::boxed(content.full_screen())
            }
            ElementType::Stack => {
                let mut stack = StackView::new();
                for child_fiber in &fiber.children {
                    match child_fiber.element.element_type {
                        ElementType::Layer => {
                            stack.add_layer(Self::create_view(
                                child_fiber.children.first().unwrap(),
                            ));
                        }
                        ElementType::FullscreenLayer => {
                            stack.add_fullscreen_layer(Self::create_view(
                                child_fiber.children.first().unwrap(),
                            ));
                        }
                        _ => panic!("Children of stacks must be layers!"),
                    }
                }
                BoxedView::boxed(stack)
            }
            ElementType::Table => {
                let mut table_view: TableView<Row, Column> = TableView::new();
                for column in fiber.element.props.columns.clone().unwrap_or_default() {
                    let align = column.align;
                    let width = column.width;
                    table_view.add_column(column, column.name, move |c| {
                        let mut c = c;
                        if let Some(align) = align {
                            c = c.align(align);
                        }
                        if let Some(width) = width {
                            c = c.width(width);
                        }
                        c
                    });
                }
                for row in fiber.element.props.rows.clone().unwrap_or_default() {
                    table_view.insert_item(row);
                }
                let on_select = fiber
                    .element
                    .props
                    .on_select
                    .clone()
                    .unwrap_or(Arc::new(Cursive::noop));
                let on_submit = fiber
                    .element
                    .props
                    .on_submit
                    .clone()
                    .unwrap_or(Arc::new(Cursive::noop));
                // TODO: callbacks should receive ref to item
                table_view.set_on_select(move |s, _, _| (on_select)(s));
                table_view.set_on_submit(move |s, _, _| (on_submit)(s));
                BoxedView::new(Box::new(table_view.full_screen()))
            }
            ElementType::VerticalLayout => {
                let mut layout = LinearLayout::vertical();
                for child_fiber in &fiber.children {
                    layout.add_child(Self::create_view(child_fiber));
                }
                BoxedView::boxed(layout)
            }
            ElementType::Leaf | ElementType::Tree => todo!(),
        }
        .with_name(fiber.name.clone())
    }

    fn update_view(view: &mut NamedView<BoxedView>, old_fiber: &Fiber, new_fiber: &Fiber) {
        // this is where we loop over children and recurse
        match old_fiber.element.element_type {
            ElementType::Button => {
                let mut button = view.get_mut();
                let button = button.get_mut::<Button>().unwrap();

                if let Some(title) = &new_fiber.element.props.title {
                    if button.label() != title.as_str() {
                        button.set_label(title.clone());
                    }
                }

                if let Some(on_submit) = &new_fiber.element.props.on_submit {
                    let on_submit = on_submit.clone();
                    button.set_callback(move |s| (on_submit)(s));
                }
            }
            ElementType::Component(_, _) => {}
            ElementType::Dialog => {
                let mut dialog = view.get_mut();
                let dialog = dialog.get_mut::<Dialog>().unwrap();

                if let Some(title) = &new_fiber.element.props.title {
                    if dialog.get_title() != title.as_str() {
                        dialog.set_title(title.clone());
                    }
                }

                let child_view = dialog
                    .get_content_mut()
                    .downcast_mut::<NamedView<BoxedView>>()
                    .unwrap();
                let old_child_fiber = old_fiber.children.first();
                let new_child_fiber = new_fiber.children.first();
                match old_child_fiber.compare(&new_child_fiber) {
                    FiberComparison::None => {}
                    FiberComparison::Removed => {
                        dialog.set_content(BoxedView::boxed(DummyView::new()).with_name("dummy"));
                    }
                    FiberComparison::Added | FiberComparison::Replaced => {
                        dialog.set_content(Self::create_view(new_child_fiber.unwrap()));
                    }
                    FiberComparison::Updated => {
                        Self::update_view(
                            child_view,
                            old_child_fiber.unwrap(),
                            new_child_fiber.unwrap(),
                        );
                    }
                }
            }
            ElementType::Hideable => {
                let old_child_fiber = old_fiber.children.first().unwrap();
                let new_child_fiber = new_fiber.children.first().unwrap();
                let mut hideable = view.get_mut();
                let hideable = hideable
                    .get_mut::<HideableView<NamedView<BoxedView>>>()
                    .unwrap();
                let child_view = hideable.get_inner_mut();
                if old_child_fiber.element.element_type == new_child_fiber.element.element_type {
                    // we have a matching old and new fiber of the same type, need to update
                    Self::update_view(child_view, old_child_fiber, new_child_fiber);
                } else {
                    // we have an old and new fiber of diff types, need to swap
                    let inner = hideable.get_inner_mut();
                    *inner = Self::create_view(new_child_fiber);
                }
            }
            ElementType::HorizontalLayout | ElementType::VerticalLayout => {
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
                    let old_child_fiber = old_fibers
                        .iter()
                        .find(|f| f.name.as_str() == child_view.name());
                    let new_child_fiber = new_fibers
                        .iter()
                        .find(|f| f.name.as_str() == child_view.name());
                    match old_child_fiber.compare(&new_child_fiber) {
                        FiberComparison::Added
                        | FiberComparison::None
                        | FiberComparison::Removed => {
                            // no old fiber for existing view, need to remove
                            // or we have an old fiber but not a new one, need to remove
                            to_remove.push(i);
                        }
                        FiberComparison::Replaced => {
                            // we have an old and new fiber of diff types, need to swap
                            to_remove.push(i);
                            let new_view = Self::create_view(new_child_fiber.unwrap());
                            layout.add_child(new_view);
                        }
                        FiberComparison::Updated => {
                            // we have a matching old and new fiber of the same type, need to update
                            Self::update_view(
                                child_view,
                                old_child_fiber.unwrap(),
                                new_child_fiber.unwrap(),
                            );
                        }
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
            ElementType::FullscreenLayer | ElementType::Layer => {
                panic!("Can't render a layer directly! Layers must be children of stacks!")
            }
            ElementType::Panel => {
                let old_child_fiber = old_fiber.children.first().unwrap();
                let new_child_fiber = new_fiber.children.first().unwrap();
                let mut panel = view.get_mut();
                let panel = panel.get_mut::<Panel<NamedView<BoxedView>>>().unwrap();
                let child_view = panel.get_inner_mut();
                if old_child_fiber.element.element_type == new_child_fiber.element.element_type {
                    // we have a matching old and new fiber of the same type, need to update
                    Self::update_view(child_view, old_child_fiber, new_child_fiber);
                } else {
                    // we have an old and new fiber of diff types, need to swap
                    let inner = panel.get_inner_mut();
                    *inner = Self::create_view(new_child_fiber);
                }
            }
            ElementType::Resized => {
                let mut resized = view.get_mut();
                let resized = resized
                    .get_mut::<ResizedView<NamedView<BoxedView>>>()
                    .unwrap();

                let old_child_fiber = old_fiber.children.first();
                let new_child_fiber = new_fiber.children.first();
                match old_child_fiber.compare(&new_child_fiber) {
                    FiberComparison::None => {}
                    FiberComparison::Removed => {
                        let inner = resized.get_inner_mut();
                        *inner = BoxedView::boxed(DummyView::new()).with_name("dummy");
                    }
                    FiberComparison::Added | FiberComparison::Replaced => {
                        let inner = resized.get_inner_mut();
                        *inner = Self::create_view(new_child_fiber.unwrap());
                    }
                    FiberComparison::Updated => {
                        let child_view = resized.get_inner_mut();
                        Self::update_view(
                            child_view,
                            old_child_fiber.unwrap(),
                            new_child_fiber.unwrap(),
                        );
                    }
                }
            }
            ElementType::Stack => {
                let old_fibers = &old_fiber.children;
                let new_fibers = &new_fiber.children;
                let mut stack = view.get_mut();
                let stack = stack.get_mut::<StackView>().unwrap();
                for i in 0..new_fibers.len() {
                    let new_fiber = &new_fibers[i];
                    let old_fiber = old_fibers.iter().find(|f| f.name == new_fiber.name);
                    let current_index = stack.find_layer_from_name(new_fiber.name.as_str());
                    match current_index {
                        Some(position) => {
                            // layer is in the ui, make sure it's ordered right and update
                            let current_index = match position {
                                LayerPosition::FromBack(i) => i,
                                LayerPosition::FromFront(i) => new_fibers.len() - i - 1,
                            };
                            if current_index != i {
                                stack.move_layer(
                                    LayerPosition::FromBack(current_index),
                                    LayerPosition::FromBack(i),
                                );
                            }

                            let layer = stack
                                .get_mut(LayerPosition::FromBack(i))
                                .unwrap()
                                .downcast_mut::<NamedView<BoxedView>>()
                                .unwrap();
                            match old_fiber.compare(&Some(new_fiber)) {
                                FiberComparison::None | FiberComparison::Removed => {}
                                FiberComparison::Added | FiberComparison::Replaced => {
                                    stack.remove_layer(LayerPosition::FromBack(i));
                                    match new_fiber.element.element_type {
                                        ElementType::FullscreenLayer => {
                                            stack.add_fullscreen_layer(Self::create_view(
                                                new_fiber.children.first().unwrap(),
                                            ));
                                        }
                                        ElementType::Layer => {
                                            stack.add_layer(Self::create_view(
                                                new_fiber.children.first().unwrap(),
                                            ));
                                        }
                                        _ => panic!("Children of stacks must be layers!"),
                                    }
                                    stack.move_layer(
                                        LayerPosition::FromFront(0),
                                        LayerPosition::FromBack(i),
                                    );
                                }
                                FiberComparison::Updated => {
                                    Self::update_view(layer, old_fiber.unwrap(), new_fiber)
                                }
                            }
                        }
                        None => {
                            // layer is not in the ui; create and insert
                            match new_fiber.element.element_type {
                                ElementType::FullscreenLayer => {
                                    stack.add_fullscreen_layer(Self::create_view(
                                        new_fiber.children.first().unwrap(),
                                    ));
                                }
                                ElementType::Layer => {
                                    stack.add_layer(Self::create_view(
                                        new_fiber.children.first().unwrap(),
                                    ));
                                }
                                _ => panic!("Children of stacks must be layers!"),
                            }
                            stack.move_layer(
                                LayerPosition::FromFront(0),
                                LayerPosition::FromBack(i),
                            );
                        }
                    }
                }

                if stack.len() > new_fibers.len() {
                    for _ in new_fibers.len()..stack.len() {
                        stack.remove_layer(LayerPosition::FromBack(new_fibers.len()));
                    }
                }
            }
            ElementType::Table => {
                let mut table = view.get_mut();
                let table = table
                    .get_mut::<ResizedView<TableView<Row, Column>>>()
                    .unwrap();
                let table = table.get_inner_mut();

                // TODO: reconcile columns?
                // reconcile rows
                let mut to_remove: Vec<usize> = vec![];
                let mut updated: Vec<String> = vec![];
                for (index, item) in table.borrow_items_mut().iter_mut().enumerate() {
                    let new_row = new_fiber
                        .element
                        .props
                        .rows
                        .as_ref()
                        .unwrap()
                        .iter()
                        .find(|row| row.id == item.id);
                    if let Some(new_row) = new_row {
                        *item = new_row.clone();
                        updated.push(new_row.id.clone());
                    } else {
                        to_remove.push(index);
                    }
                }
                for index in to_remove {
                    table.remove_item(index);
                }
                for new_row in new_fiber
                    .element
                    .props
                    .rows
                    .as_ref()
                    .unwrap()
                    .iter()
                    .filter(|row| !updated.iter().any(|s| *s == row.id))
                {
                    table.insert_item(new_row.clone());
                }

                // reconcile callbacks
                let on_select = new_fiber
                    .element
                    .props
                    .on_select
                    .clone()
                    .unwrap_or(Arc::new(Cursive::noop));
                let on_submit = new_fiber
                    .element
                    .props
                    .on_submit
                    .clone()
                    .unwrap_or(Arc::new(Cursive::noop));
                table.set_on_select(move |s, _, _| (on_select)(s));
                table.set_on_submit(move |s, _, _| (on_submit)(s));
            }
            ElementType::Leaf | ElementType::Tree => todo!(),
            ElementType::Menubar => {}
        }
    }

    pub fn use_context<T>(
        &mut self,
        key: String,
        initial_value: T,
    ) -> (T, Arc<dyn Fn(T) + Send + Sync>)
    where
        T: Clone + Send + Sync + 'static,
    {
        let cb_sink = self.cb_sink.as_ref().unwrap().clone();
        let state = Arc::clone(&self.state);

        // Set initial value if it doesn't exist
        {
            state
                .as_ref()
                .write()
                .unwrap()
                .entry(key.clone())
                .or_insert_with(|| Box::new(initial_value.clone()) as Box<StateValue>);
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

    pub fn use_state<T>(&mut self, initial_value: T) -> (T, Arc<dyn Fn(T) + Send + Sync>)
    where
        T: Clone + Send + Sync + 'static,
    {
        let key = format!("{}.{}", self.element_key, self.state_index);
        self.state_index += 1;
        self.use_context(key, initial_value)
    }
}

impl Default for React {
    fn default() -> Self {
        Self::new()
    }
}

fn main() {
    let react = React::new();
    react.render(component(sisko_app));
}

fn app(react: &mut React) -> Option<Element> {
    let (count, set_count) = react.use_state(0);
    Some(stack(vec![dialog(
        "Test Title",
        vec![props_component(
            counter,
            CounterProps {
                count,
                on_submit: Arc::new(move |_| set_count(count + 1)),
            },
        )],
    )]))
}

struct CounterProps {
    pub count: i32,
    pub on_submit: Arc<Callback>,
}

fn counter(_react: &mut React, props: &CounterProps) -> Option<Element> {
    let CounterProps { count, on_submit } = props;
    Some(button(format!("{}", count), on_submit.clone()))
}

fn sisko_app(react: &mut React) -> Option<Element> {
    let (show_modal, set_show_modal) = react.use_state(false);
    let set_show_modal_copy = set_show_modal.clone();
    let mut layers: Vec<Element> = vec![fullscreen_layer(vertical_layout(vec![
        menubar(vec![
            tree(
                "File",
                vec![
                    leaf("Add Folder", Arc::new(|_| {})),
                    leaf("Add Files", Arc::new(|_| {})),
                    leaf(
                        "Exit",
                        Arc::new(|s| {
                            s.quit();
                        }),
                    ),
                ],
            ),
            tree("Edit", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("View", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Options", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Tools", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Help", vec![leaf("About", Arc::new(|_| {}))]),
        ]),
        horizontal_layout(vec![component(left_panel), component(right_panel)]),
        component(bottom_panel),
        props_component(
            bottom_buttons,
            ButtonProps {
                on_submit: Arc::new(move |_| set_show_modal(true)),
            },
        ),
    ]))];
    if show_modal {
        layers.push(layer(dialog(
            "Test Modal",
            vec![button(
                "Okay".to_string(),
                Arc::new(move |_| set_show_modal_copy(false)),
            )],
        )))
    }
    Some(stack(layers))
}

fn left_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![],
        vec![],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

fn right_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![],
        vec![],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

fn bottom_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![
            Column::new("Tag"),
            Column::new("Original Value"),
            Column::new("New Value"),
        ],
        vec![Row::new(
            "0",
            HashMap::from([
                ("Tag", String::from("Test")),
                ("Original Value", String::from("orig")),
                ("New Value", String::from("new")),
            ]),
        )],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

struct ButtonProps {
    on_submit: Arc<Callback>,
}

fn bottom_buttons(_: &mut React, props: &ButtonProps) -> Option<Element> {
    let ButtonProps { on_submit } = props;
    Some(horizontal_layout(vec![
        button(String::from("Add Folder"), on_submit.clone()),
        button(String::from("Add Files"), Arc::new(Cursive::noop)),
        button(String::from("Cluster"), Arc::new(Cursive::noop)),
        button(String::from("Lookup"), Arc::new(Cursive::noop)),
        button(String::from("Scan"), Arc::new(Cursive::noop)),
        button(String::from("Save"), Arc::new(Cursive::noop)),
        button(String::from("Info"), Arc::new(Cursive::noop)),
        button(String::from("Remove"), Arc::new(Cursive::noop)),
        button(String::from("Lookup CD"), Arc::new(Cursive::noop)),
    ]))
}

fn stack_app(react: &mut React) -> Option<Element> {
    let (count, set_count) = react.use_state(0);
    let set_count_b = set_count.clone();
    let mut dialog_a = resized(dialog(
        "Dialog A",
        vec![button(
            String::from("Swap"),
            Arc::new(move |_| set_count(1)),
        )],
    ));
    let mut dialog_b = resized(dialog(
        "Dialog B",
        vec![button(
            String::from("Swap"),
            Arc::new(move |_| set_count_b(0)),
        )],
    ));
    dialog_a.props.key = Some("dialog_a".to_string());
    dialog_b.props.key = Some("dialog_b".to_string());
    let layers = if count == 0 {
        vec![dialog_b, dialog_a]
    } else {
        vec![dialog_a, dialog_b]
    };
    Some(stack(layers))
}
