/**
 * Copyright (c) 2017-2024 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.apache.mina.util.AvailablePortFinder;
import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.se_flowable.unit_test.start_stop.Start;
import org.ow2.petals.se_flowable.unit_test.start_stop.StartResponse;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;

/**
 * Abstract class for unit tests about process invoking InOnly or RobustInOnly services
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class StartStopTestEnvironment extends AbstractTestEnvironment {

    protected static final String START_STOP_SU = "start-stop";

    private static final String START_STOP_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/start-stop";

    protected static final QName START_STOP_INTERFACE = new QName(START_STOP_NAMESPACE, "start-stop");

    protected static final QName START_STOP_SERVICE = new QName(START_STOP_NAMESPACE, "start-stop-service");

    protected static final String START_STOP_ENDPOINT = "edpStartStop";

    protected static final QName OPERATION_START = new QName(START_STOP_NAMESPACE, "start");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "start-stop";

    protected static int TIMER_DURATION_S = 10;

    protected static int TIMER_DURATION_MS = TIMER_DURATION_S * 1000;

    protected static String getFileIdmEngineConfiguratorCfgFile() {
        return null;
    }

    // Don't start the component automatically
    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest(true, false)
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                    FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME), "1000")
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP,
                    FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME), "1000")
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(
                            AvailablePortFinder.getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT)))
            .registerServiceToDeploy(START_STOP_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/start-stop/start-stop.wsdl");
                    assertNotNull("WSDL not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            START_STOP_INTERFACE, START_STOP_SERVICE, START_STOP_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/start-stop/startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/start-stop/start-stop.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "start-stop.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    return serviceConfiguration;
                }
            });

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Start.class, StartResponse.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected static byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            MARSHALLER.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }
}
