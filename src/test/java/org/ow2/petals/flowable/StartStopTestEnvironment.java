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

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about process invoking InOnly or RobustInOnly services
 * 
 * @author Christophe DENEUX - Linagora
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

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext
                .newInstance(org.ow2.petals.se_flowable.unit_test.start_stop.ObjectFactory.class);
        UNMARSHALLER = context.createUnmarshaller();
        MARSHALLER = context.createMarshaller();
        MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    }

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();
        completesComponentUnderTestConfiguration();
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {

        COMPONENT_UNDER_TEST.componentMustBeStarted(false)
                .registerServiceToDeploy(START_STOP_SU, new ServiceConfigurationFactory() {
                    @Override
                    public ServiceConfiguration create() {

                        final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/start-stop/start-stop.wsdl");
                        assertNotNull(wsdlUrl, "WSDL not found");
                        final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                                START_STOP_INTERFACE, START_STOP_SERVICE, START_STOP_ENDPOINT, wsdlUrl);

                        final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/start-stop/startResponse.xsl");
                        assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                        serviceConfiguration.addResource(startResponseXslUrl);

                        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/start-stop/start-stop.bpmn");
                        assertNotNull(bpmnUrl, "BPMN file not found");
                        serviceConfiguration.addResource(bpmnUrl);

                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "start-stop.bpmn");
                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                        return serviceConfiguration;
                    }
                }).postInitComponentUnderTest();
    }
}
