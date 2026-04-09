import DefaultTheme from 'vitepress/theme'
import './style.css'
import CsvTable from './components/CsvTable.vue'

export default {
  ...DefaultTheme,
  enhanceApp({ app }) {
    app.component('CsvTable', CsvTable)
  }
}