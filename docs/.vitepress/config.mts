import { defineConfig } from 'vitepress'

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "Infection and Brain",
  description: "",
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Examples', link: '/markdown-examples' },
      { text: 'Code', link: 'https://github.com/Dapu-Li/Infection_and_Brain' },
      { text: 'Results',
        items: [
          { text: 'Brain Age Gap', link: '/s0_BAG/Overview/' },
          { text: 'Brain Structure', link: '/s1_Structure/Overview/' },
          { text: 'Brain Disease', link: '/s2_Disease/Overview/' },
          { text: 'Brain Function', link: '/s3_Function/Overview/' },
        ]
      }
    ],

    sidebar: {
      '/s0_BAG/':[
      {
        text: 'Brain Age Gap', link: '/s0_BAG/Overview/',
        collapsed: false,
        items: [
          { text: 'Results', link: '/s0_BAG/MD/BAG/' },
        ]
      }
    ],

      '/s1_Structure/':[
      {
        text: 'Infection Type', link: '/s1_Structure/Overview/',
        collapsed: false,
        items: [
          { text: 'All Infectious Diseases', link: '/s1_Structure/MD/All Infectious Diseases/' },
          { text: 'Bacterial Infections', link: '/s1_Structure/MD/Bacterial Infections/' },
          { text: 'Invasive Bacterial Infections', link: '/s1_Structure/MD/Invasive Bacterial Infections/' },
          { text: 'Localised Bacterial Infections', link: '/s1_Structure/MD/Localised Bacterial Infections/' },
          { text: 'Bacterial Infections with Sepsis', link: '/s1_Structure/MD/Bacterial Infections with Sepsis/' },
          { text: 'Bacterial Infections without Sepsis', link: '/s1_Structure/MD/Bacterial Infections without Sepsis/' },
          { text: 'Extracellular Bacterial Infections', link: '/s1_Structure/MD/Extracellular Bacterial Infections/' },
          { text: 'Intracellular Bacterial Infections', link: '/s1_Structure/MD/Intracellular Bacterial Infections/' },
          { text: 'Gram Positive Bacterial Infections', link: '/s1_Structure/MD/Gram Positive Bacterial Infections/' },
          { text: 'Gram Negative Bacterial Infections', link: '/s1_Structure/MD/Gram Negative Bacterial Infections/' },
          { text: 'Mycobacterial Infections', link: '/s1_Structure/MD/Mycobacterial Infections/' },
          { text: 'Mycoplasma Infections', link: '/s1_Structure/MD/Mycoplasma Infections/' },
          { text: 'Acute Bacterial Infections', link: '/s1_Structure/MD/Acute Bacterial Infections/' },
          { text: 'Chronic Bacterial Infections', link: '/s1_Structure/MD/Chronic Bacterial Infections/' },
          { text: 'Viral Infections', link: '/s1_Structure/MD/Viral Infections/' },
          { text: 'Acute Viral Infections', link: '/s1_Structure/MD/Acute Viral Infections/' },
          { text: 'Herpes Viral Infections', link: '/s1_Structure/MD/Herpes Viral Infections/' },
          { text: 'Other Potentially Persistent Viral Infections', link: '/s1_Structure/MD/Other Potentially Persistent Viral Infections/' },
          { text: 'Parasitic Infections', link: '/s1_Structure/MD/Parasitic Infections/' },
          { text: 'Fungal Infections', link: '/s1_Structure/MD/Fungal Infections/' },
          { text: 'Acute Infections', link: '/s1_Structure/MD/Acute Infections/' },
          { text: 'Chronic Infections', link: '/s1_Structure/MD/Chronic Infections/' },
        ]
      }
    ],
      '/s2_Disease/':[
        {
          text: 'Disease Type', link: '/s2_Disease/Overview/',
          collapsed: false,
          //['ACD', 'AD', 'VD', 'PD', 'Stroke', 'Depression', 'Anxiety']
          items: [
            { text: 'ACD', link: '/s2_Disease/MD/ACD/' },
            { text: 'AD', link: '/s2_Disease/MD/AD/' },
            { text: 'VD', link: '/s2_Disease/MD/VD/' },
            { text: 'PD', link: '/s2_Disease/MD/PD/' },
            { text: 'Stroke', link: '/s2_Disease/MD/Stroke/' },
            { text: 'Depression', link: '/s2_Disease/MD/Depression/' },
            { text: 'Anxiety', link: '/s2_Disease/MD/Anxiety/' }
          ]
        }
      ],
      '/s3_Function/':[
        {
          text: 'Function Type', link: '/s3_Function/Overview/',
          collapsed: false,
          items: [
            { text: 'Numeric memory', link: '/s3_Function/MD/Numeric memory/' },
            { text: 'Fluid intelligence', link: '/s3_Function/MD/Fluid intelligence/' },
            { text: 'Prospective memory', link: '/s3_Function/MD/Prospective memory/' },
            { text: 'Tower rearranging', link: '/s3_Function/MD/Tower rearranging/' },
            { text: 'Symbol digit substitution', link: '/s3_Function/MD/Symbol digit substitution/' },
            { text: 'Paired associate learning', link: '/s3_Function/MD/Paired associate learning/' },
            { text: 'Reaction time', link: '/s3_Function/MD/Reaction time/' },
            { text: 'Pairs matching', link: '/s3_Function/MD/Pairs matching/' },
            { text: 'Trail making 1', link: '/s3_Function/MD/Trail making 1/' },
            { text: 'Trail making 2', link: '/s3_Function/MD/Trail making 2/' },
            { text: 'Hand grip strength left', link: '/s3_Function/MD/Hand grip strength left/' },
            { text: 'Hand grip strength right', link: '/s3_Function/MD/Hand grip strength right/' },
            { text: 'Usual walking pace', link: '/s3_Function/MD/Usual walking pace/' },
            { text: 'Falls in the last year', link: '/s3_Function/MD/Falls in the last year/' }
          ]
        }
      ],
    },
    footer: {
      copyright: 'Copyright © 2026 Zhirong Li'
    },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/Dapu-Li'}
    ]
  }
})
