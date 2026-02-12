/**
 * Copyright (c) 2019-2026 Linagora
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
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class IntermediateMessageCatchEventLoopProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_SU = "intermediate-message-catch-event-loop-su";

    private static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/intermediate-message-catch-event-loop";

    protected static final QName INTERMEDIATE_MESSAGE_CATCH_EVENT_INTERFACE = new QName(
            INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "intermediate-message-catch-event-loop");

    protected static final QName INTERMEDIATE_MESSAGE_CATCH_EVENT_SERVICE = new QName(
            INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "intermediate-message-catch-event-loop-service");

    protected static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_ENDPOINT = "edpIntermediateMessageCatchEventLoop";

    protected static final QName OPERATION_START = new QName(INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "unlock");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "intermediate-message-catch-event-loop";

    protected static final String BPMN_USER = "kermit";

    protected static final String MESSAGE_EVENT_NAME = "myMessageName";

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event_loop.ObjectFactory.class,
                org.ow2.petals.components.flowable.generic._1.ObjectFactory.class);
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
        COMPONENT_UNDER_TEST
                .registerServiceToDeploy(INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, new ServiceConfigurationFactory() {
                    @Override
                    public ServiceConfiguration create() {

                        final URL wsdlUrl = Thread.currentThread().getContextClassLoader().getResource(
                                "su/intermediate-message-catch-event/intermediate-message-catch-event-loop.wsdl");
                        assertNotNull(wsdlUrl, "WSDL not found");
                        final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                                INTERMEDIATE_MESSAGE_CATCH_EVENT_INTERFACE, INTERMEDIATE_MESSAGE_CATCH_EVENT_SERVICE,
                                INTERMEDIATE_MESSAGE_CATCH_EVENT_ENDPOINT, wsdlUrl);

                        final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/intermediate-message-catch-event/startResponse.xsl");
                        assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                        serviceConfiguration.addResource(startResponseXslUrl);

                        final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/intermediate-message-catch-event/instanceUnknown.xsl");
                        assertNotNull(instanceUnknownXslUrl, "Output XSL 'instanceUnknown.xsl' not found");
                        serviceConfiguration.addResource(instanceUnknownXslUrl);

                        final URL unexpectedMessageEventXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/intermediate-message-catch-event/unexpectedMessageEvent.xsl");
                        assertNotNull(unexpectedMessageEventXslUrl,
                                "Output XSL 'unexpectedMessageEvent.xsl' not found");
                        serviceConfiguration.addResource(unexpectedMessageEventXslUrl);

                        final URL messageEventReceivedXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("su/intermediate-message-catch-event/messageEventReceived.xsl");
                        assertNotNull(messageEventReceivedXslUrl, "Output XSL 'messageEventReceived.xsl' not found");
                        serviceConfiguration.addResource(messageEventReceivedXslUrl);

                        final URL bpmnUrl = Thread.currentThread().getContextClassLoader().getResource(
                                "su/intermediate-message-catch-event/intermediate-message-catch-event-loop.bpmn");
                        assertNotNull(bpmnUrl, "BPMN file not found");
                        serviceConfiguration.addResource(bpmnUrl);

                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"),
                                "intermediate-message-catch-event-loop.bpmn");
                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                        return serviceConfiguration;
                    }
                }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG,
                        new NativeServiceConfigurationFactory() {

                            @Override
                            public ServiceConfiguration create(final String nativeEndpointName) {

                                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                        .getResource("component.wsdl");
                                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                                return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                                        ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                            }

                            @Override
                            public QName getNativeService() {
                                return ITG_PROCESSINSTANCES_SERVICE;
                            }
                        })
                .registerNativeServiceToDeploy(NATIVE_EXECUTIONS_SVC_CFG, new NativeServiceConfigurationFactory() {

                    @Override
                    public ServiceConfiguration create(final String nativeEndpointName) {

                        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("component.wsdl");
                        assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                        return new ProvidesServiceConfiguration(ITG_EXECUTIONS_PORT_TYPE, ITG_EXECUTIONS_SERVICE,
                                nativeEndpointName, nativeServiceWsdlUrl);
                    }

                    @Override
                    public QName getNativeService() {
                        return ITG_EXECUTIONS_SERVICE;
                    }
                }).postInitComponentUnderTest();
    }
}
