/**
 * Copyright (c) 2014-2019 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.apache.mina.util.AvailablePortFinder;
import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.AlreadyUnlocked;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.NotLocked;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Start;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.StartResponse;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Unlock;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class IntermediateMessageCatchEventProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_SU = "intermediate-message-catch-event-su";

    private static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/intermediate-message-catch-event";

    protected static final QName INTERMEDIATE_MESSAGE_CATCH_EVENT_INTERFACE = new QName(
            INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "intermediate-message-catch-event");

    protected static final QName INTERMEDIATE_MESSAGE_CATCH_EVENT_SERVICE = new QName(
            INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "intermediate-message-catch-event-service");

    protected static final String INTERMEDIATE_MESSAGE_CATCH_EVENT_ENDPOINT = "edpIntermediateMessageCatchEvent";

    protected static final QName OPERATION_START = new QName(INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(INTERMEDIATE_MESSAGE_CATCH_EVENT_NAMESPACE, "unlock");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "intermediate-message-catch-event";

    protected static final String BPMN_USER = "kermit";

    protected static final String USER_TASK_1 = "userTask1";

    protected static final String USER_TASK_2 = "userTask2";

    protected static String getFileIdmEngineConfiguratorCfgFile() {
        return null;
    }

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            // A async job executor is required to process service task
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(
                            AvailablePortFinder.getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT)))
            .registerServiceToDeploy(INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/intermediate-message-catch-event.wsdl");
                    assertNotNull("WSDl not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            INTERMEDIATE_MESSAGE_CATCH_EVENT_INTERFACE, INTERMEDIATE_MESSAGE_CATCH_EVENT_SERVICE,
                            INTERMEDIATE_MESSAGE_CATCH_EVENT_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/instanceUnknown.xsl");
                    assertNotNull("Output XSL 'instanceUnknown.xsl' not found", instanceUnknownXslUrl);
                    serviceConfiguration.addResource(instanceUnknownXslUrl);

                    final URL unexpectedMessageEventXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/unexpectedMessageEvent.xsl");
                    assertNotNull("Output XSL 'unexpectedMessageEvent.xsl' not found", unexpectedMessageEventXslUrl);
                    serviceConfiguration.addResource(unexpectedMessageEventXslUrl);

                    final URL messageEventReceivedXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/messageEventReceived.xsl");
                    assertNotNull("Output XSL 'messageEventReceived.xsl' not found", messageEventReceivedXslUrl);
                    serviceConfiguration.addResource(messageEventReceivedXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/intermediate-message-catch-event/intermediate-message-catch-event.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"),
                            "intermediate-message-catch-event.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    return serviceConfiguration;
                }
            }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                            ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_PROCESSINSTANCES_SERVICE;
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
            final JAXBContext context = JAXBContext.newInstance(Start.class, StartResponse.class, Unlock.class,
                    NotLocked.class, AlreadyUnlocked.class);
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
