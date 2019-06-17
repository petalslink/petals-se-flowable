/**
 * Copyright (c) 2017-2019 Linagora
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
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.se_flowable.unit_test.placeholders.Start;
import org.ow2.petals.se_flowable.unit_test.placeholders.StartResponse;
import org.ow2.petals.se_flowable.unit_test.placeholders.Unlock;
import org.ow2.petals.se_flowable.unit_test.placeholders.UnlockAck;
import org.ow2.petals.se_flowable.unit_test.placeholders.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.placeholders.archivageservice.ArchiverResponse;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class PlaceholdersProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String PLACEHOLDERS_SU = "placeholders";

    protected static final String PLACEHOLDERS_SU_HOME = "su/placeholders/";

    private static final String PLACEHOLDERS_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/placeholders";

    protected static final QName PLACEHOLDERS_INTERFACE = new QName(PLACEHOLDERS_NAMESPACE, "placeholders");

    protected static final QName PLACEHOLDERS_SERVICE = new QName(PLACEHOLDERS_NAMESPACE, "placeholders-service");

    protected static final String PLACEHOLDERS_ENDPOINT = "edpPlaceholders";

    protected static final QName OPERATION_START = new QName(PLACEHOLDERS_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(PLACEHOLDERS_NAMESPACE, "unlock");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/placeholders/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "placeholders";

    protected static final String BPMN_USER = "kermit";

    protected static final String USER_TASK_1 = "userTask1";

    protected static final String USER_TASK_2 = "userTask2";

    protected static final String USER_TASK_3 = "userTask3";

    protected static String getFileIdmEngineConfiguratorCfgFile() {
        return null;
    }

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(
                            AvailablePortFinder.getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT)))
            .setParameter(new QName("http://petals.ow2.org/components/extensions/version-5", "properties-file"),
                    new ParameterGenerator() {

                        @Override
                        public String generate() throws Exception {
                            final URL componentPropertiesFile = Thread.currentThread().getContextClassLoader()
                                    .getResource("su/placeholders/componentProperties.properties");
                            assertNotNull("Component properties file is missing !", componentPropertiesFile);
                            return componentPropertiesFile.toString();
                        }

                    })
            .registerServiceToDeploy(PLACEHOLDERS_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "placeholders.wsdl");
                    assertNotNull("WSDL not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            PLACEHOLDERS_INTERFACE, PLACEHOLDERS_SERVICE, PLACEHOLDERS_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL unlockResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "unlockAckResponse.xsl");
                    assertNotNull("Output XSL 'unlockAckResponse.xsl' not found", unlockResponseXslUrl);
                    serviceConfiguration.addResource(unlockResponseXslUrl);

                    final URL alreadyUnlockedXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "alreadyUnlocked.xsl");
                    assertNotNull("Output XSL 'alreadyUnlocked.xsl' not found", alreadyUnlockedXslUrl);
                    serviceConfiguration.addResource(alreadyUnlockedXslUrl);

                    final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "instanceUnknown.xsl");
                    assertNotNull("Output XSL 'instanceUnknown.xsl' not found", instanceUnknownXslUrl);
                    serviceConfiguration.addResource(instanceUnknownXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "placeholders.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource(PLACEHOLDERS_SU_HOME + "archivageService.wsdl");
                    assertNotNull("archivageService WSDL not found", archivageServiceWsdlUrl);
                    serviceConfiguration.addResource(archivageServiceWsdlUrl);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "placeholders.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    return serviceConfiguration;
                }
            }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE);

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Start.class, StartResponse.class, Unlock.class,
                    UnlockAck.class, Archiver.class, ArchiverResponse.class);
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
