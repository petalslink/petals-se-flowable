/**
 * Copyright (c) 2014 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;

import org.junit.Test;
import org.ow2.petals.component.framework.JBIBootstrap;
import org.ow2.petals.component.framework.jbidescriptor.CDKJBIDescriptorException;
import org.ow2.petals.component.framework.jbidescriptor.JBIDescriptorBuilder;
import org.ow2.petals.component.framework.jbidescriptor.generated.Component;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

/**
 * Unit tests of {@link ActivitiSEBootstrap}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ActivitiSEBootstrapTest {

    /**
     * Check that the component embeds the right hard-coded default configuration (values of the component JBI
     * descriptor are set to null or empty)
     */
    @Test
    public void defaultConfiguration_definedInComponentSourceCode() throws SecurityException, NoSuchFieldException,
            IllegalArgumentException, IllegalAccessException {

        // Create a minimalist JBI descriptor
        final Jbi jbiComponentConfiguration = new Jbi();
        final Component component = new Component();
        jbiComponentConfiguration.setComponent(component);
        final List<Element> params = component.getAny();

        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document doc = docBuilder.newDocument();
        final Element eltJdbcMaxActiveConnections = doc.createElementNS("",
                ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        params.add(eltJdbcMaxActiveConnections);
        final Element eltJdbcMaxIdleConnections = doc.createElementNS("",
                ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        params.add(eltJdbcMaxIdleConnections);
        final Element eltJdbcMaxCheckoutTime = doc.createElementNS("",
                ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        params.add(eltJdbcMaxCheckoutTime);
        final Element eltJdbcMaxWaitTime = doc.createElementNS("", ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        params.add(eltJdbcMaxWaitTime);

        this.assertDefaultValue(this.createActivitSEBootstrap(jbiComponentConfiguration));
    }

    /**
     * Check that the component embeds the right default configuration in its JBI descriptor (values set to their
     * default value in jbi.xml)
     */
    @Test
    public void defaultConfiguration_definedInJbiDescriptor() throws CDKJBIDescriptorException, SecurityException,
            NoSuchFieldException, IllegalArgumentException, IllegalAccessException {

        final InputStream defaultJbiDescriptorStream = Thread.currentThread().getContextClassLoader()
                .getResourceAsStream("jbi/jbi.xml");
        assertNotNull("The component JBI descriptor is missing", defaultJbiDescriptorStream);
        final Jbi jbiComponentConfiguration = JBIDescriptorBuilder.buildJavaJBIDescriptor(defaultJbiDescriptorStream);

        this.assertDefaultValue(this.createActivitSEBootstrap(jbiComponentConfiguration));
    }

    /**
     * Create a boostrap of the component
     * 
     * @param jbiComponentConfiguration
     *            The JBI descriptor to use
     * @return
     */
    private ActivitiSEBootstrap createActivitSEBootstrap(final Jbi jbiComponentConfiguration)
            throws IllegalArgumentException, IllegalAccessException, SecurityException, NoSuchFieldException {

        final ActivitiSEBootstrap bootstrap = new ActivitiSEBootstrap();
        final Field jbiComponentCfgField = JBIBootstrap.class.getDeclaredField("jbiComponentConfiguration");
        jbiComponentCfgField.setAccessible(true);
        jbiComponentCfgField.set(bootstrap, jbiComponentConfiguration);
        final Field loggerField = JBIBootstrap.class.getDeclaredField("logger");
        loggerField.setAccessible(true);
        loggerField.set(bootstrap, Logger.getLogger(this.getClass().getName()));

        return bootstrap;
    }

    private void assertDefaultValue(final ActivitiSEBootstrap bootstrap) {

        assertEquals(ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS,
                bootstrap.getJdbcMaxActiveConnections());
        assertEquals(ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS,
                bootstrap.getJdbcMaxIdleConnections());
        assertEquals(ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME, bootstrap.getJdbcMaxCheckoutTime());
        assertEquals(ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME, bootstrap.getJdbcMaxWaitTime());
    }

}
